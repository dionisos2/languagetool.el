;; Fonction utilitaire pour votre développement
(defun my-reload-languagetool-tests ()
	"Reload languagetool package and tests cleanly."
	(interactive)
	;; Décharger le paquet principal
	(unload-feature 'languagetool t)
	(unload-feature 'languagetool-server t)
	(unload-feature 'languagetool-correction t)
	(unload-feature 'languagetool-core t)
	;; Supprimer les anciens tests
	(ert-delete-all-tests)
	(load "test.el")
	(message "LanguageTool package and tests reloaded"))
