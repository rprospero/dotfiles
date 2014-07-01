(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "work"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))


(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "professional"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))
