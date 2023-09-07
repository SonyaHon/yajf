compile: test.yaj
	make clean
	cargo run -- transpile ./test.yaj	

clean:
	rm -rf ./bundle.js ./_o_.bundle.js
