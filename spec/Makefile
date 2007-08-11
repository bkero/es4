dummy: library.html

library.html: stitch.py part3.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part3.html library.html

es4.html: stitch.py spec.html es4.css overview/*.html language/*.html library/*.html ../builtins/*.es ../*.sml
	python stitch.py spec.html es4.html

clean:
	rm -f es4.html library.html *~
