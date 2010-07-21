(use 'compojure)

(defroutes arc-challenge
  (GET "/favicon.ico" nil)
  (GET "/sayit/"  (html
                   (form-to [:post "/click/"]
                     (text-field "saying" "")
                     (submit-button "Say it"))))
  (GET "/click/"  (html (str "You said: " (:message session))))
  (POST "/click/" [(session-assoc :message (:saying params))
                   (html (link-to "/click/" "Click here"))]))

(run-server {:port 8080} "/*"
            (servlet
             (with-session arc-challenge)))
