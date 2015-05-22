test:
	go test ./...
build: test
	go build -o mm
run: build
	./mm $@
