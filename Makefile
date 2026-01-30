.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/caisse.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/caisse.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release caisse.html
	xdg-open _build/default/caisse.html
