; block      let*                  return-from      
; catch      load-time-value       setq             
; eval-when  locally               symbol-macrolet  
; flet       macrolet              tagbody          
; function   multiple-value-call   the              
; go         multiple-value-prog1  throw            
; if         progn                 unwind-protect   
; labels     progv                                  
; let        quote                                  


(ql-dist:install-dist "http://dist.shirakumo.org/shirakumo.txt")

(ql:quickload :trial)


(require :my-trial-project)