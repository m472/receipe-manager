format:
	black .
	isort .
	elm-format --yes frontend/src

dev-build-frontend:
	cd frontend; elm make --output ../static/Main.js src/Main.elm

run-dev: dev-build-frontend
	python main.py

test-elm:
	cd frontend; npx elm-test
