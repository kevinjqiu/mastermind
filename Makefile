test:
	go test -v ./...
build: test
	go build -o mm
run: build
	./mm $@
