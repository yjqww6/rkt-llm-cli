from aiohttp import web
import asyncio
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
from pydantic_core import to_jsonable_python
import argparse, os, json

def convert_to_openai_tool(tool_definition):
    def convert_schema(schema):
        if isinstance(schema, dict) and "properties" in schema:
            return {
                "type": "object",
                "properties": {k: convert_schema(v) for k, v in schema["properties"].items()},
                "required": schema.get("required", [])
            }
        elif isinstance(schema, dict) and ("oneOf" in schema or "anyOf" in schema):
            key = "oneOf" if "oneOf" in schema else "anyOf"
            return {
                key: [convert_schema(item) for item in schema[key]]
            }
        elif isinstance(schema, dict) and schema["type"] == "array":
            return {
                "type": "array",
                "items": convert_schema(schema["items"])
            }
        else:
            return schema
    
    openai_tool = {
        "name": tool_definition["name"],
        "description": tool_definition.get("description", ""),
        "parameters": convert_schema(tool_definition["inputSchema"])
    }
    
    return openai_tool

async def get_tools(request):
    cached_tools = request.app['cached_tools']
    return web.json_response({"tools": cached_tools})

async def call_tool(request):
    app = request.app
    try:
        data = await request.json()
    except json.JSONDecodeError:
        return web.json_response({'error': 'Invalid JSON'}, status=400)
    
    required = ['name', 'arguments']
    missing = [f for f in required if f not in data]
    if missing:
        return web.json_response({'error': f"Missing fields: {', '.join(missing)}"}, status=400)
    
    name = data['name']
    arguments = data['arguments']
    
    session = app['tool_to_session'].get(name)
    if not session:
        return web.json_response({'error': f"Tool '{name}' not found"}, status=404)
    
    try:
        result = await session.call_tool(name, arguments)
    except Exception as e:
        return web.json_response({'error': f"Tool Call Failed: {str(e)}"}, status=500)
    return web.json_response(to_jsonable_python(result))

# Configure multiple MCP service parameters
def get_server_params_list(allowed_paths, env=None):
    server_params_list = [
        StdioServerParameters(command="python", args=["-m", "mcp_server_fetch"], env=env)
    ]

    if allowed_paths:
        server_params_list.append(
            StdioServerParameters(
                command="npx",
                args=["-y", "@modelcontextprotocol/server-filesystem"] + allowed_paths,
                env=env
            )
        )

    return server_params_list

async def init_app(server_list):
    app = web.Application()
    app.router.add_get('/tools', get_tools)
    app.router.add_post('/tool_call', call_tool)

    sessions = []
    stdio_managers = []
    session_managers = []
    tool_to_session = {}
    cached_tools = []

    # Initialize all MCP sessions and cache the tool list
    for params in server_list:
        manager = stdio_client(params)
        try:
            read, write = await manager.__aenter__()
        except Exception as e:
            print(f"Failed to start session for {params}: {e}")
            continue
        stdio_managers.append(manager)
        session = ClientSession(read, write)
        se = await session.__aenter__()
        session_managers.append(se)
        await session.initialize()
        sessions.append(session)
        
        # Get the current session's tool list and cache it
        try:
            tools = await session.list_tools()
            tools_list = to_jsonable_python(tools.tools)
            for tool in tools_list:
                tool_name = tool["name"]
                if tool_name in tool_to_session:
                    # Handle tool name conflicts (override and warn)
                    print(f"Warning: Tool '{tool_name}' already exists. Overriding.")
                tool_to_session[tool_name] = session
            cached_tools.extend(tools_list)
        except Exception as e:
            print(f"Error listing tools for session {params}: {e}")
            await session.close()
            stdio_managers.remove(manager)
            sessions.remove(session)
            continue

    # Store session information and cached tool list in the application object
    app['mcp_sessions'] = sessions
    app['stdio_managers'] = stdio_managers
    app['tool_to_session'] = tool_to_session
    app['cached_tools'] = [convert_to_openai_tool(tool) for tool in cached_tools]

    # Start HTTP server
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, 'localhost', 9876)
    await site.start()
    print("Server started successfully")

    try:
        # Block until manually stopped
        await asyncio.Event().wait()
    finally:
        # Clean up all resources
        for se in session_managers:
            await se.__aexit__(None, None, None)
        for manager in stdio_managers:
            await manager.__aexit__(None, None, None)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='MCP Server')
    parser.add_argument('-d', '--directory', action='append', help='Allowed directory paths for the filesystem MCP server')
    args = parser.parse_args()

    allowed_paths = args.directory if args.directory else []

    asyncio.run(init_app(get_server_params_list(allowed_paths)))