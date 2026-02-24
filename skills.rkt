#lang racket
(require racket/string racket/file file/glob
         "tools.rkt")
(provide make-skill-tool)

(struct Skill (name front-matter content path))

(define (parse-skill path)
  (match-define (list _ frontmatter content)
    (regexp-match #px"---\n(.*?)\n---\n(.*)$" (file->string path)))
  (match-define (list _ name) (regexp-match #px"name: (\\S+?)\\s" frontmatter))
  (Skill name frontmatter content path))

(define (scan-skills path)
  (glob (build-path path "**" "SKILL.md")))

(define (make-desc skills)
  (format
   #<<TPL
Load a skill with domain specific knowledge.
<available_skills>
~a
</available_skills>
TPL
   (string-join
    (for/list ([skill (in-list skills)])
      (format #<<TPL
<skill>
~a
location: ~a
</skill>

TPL
              (Skill-front-matter skill) (simplify-path (build-path (Skill-path skill) 'up)))))))

(define (load-skills path)
  (for/list ([s (in-list (scan-skills path))])
    (parse-skill s)))

(define (make-skill-tool skills-path)
  (define skills (load-skills skills-path))
  (define-tool (skill [name : string #:desc "The name of the skill from available_skills"])
    #:desc (make-desc skills)
    (define skill
      (findf
       (lambda (s) (string=? name (Skill-name s)))
       skills))
    (cond
      [(not skill) "Error: skill not found"]
      [else
       (format
        #<<TPL
<skill_content>
~a
</skill_content>
<skill_files>
~a
</skill_files>
TPL
        (Skill-content skill)
        (string-join
         (map path->string (glob (build-path (Skill-path skill) 'up "**" "*")))
         "\n"))]))
  skill)