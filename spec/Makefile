dummy: language.html library.html

language.html: stitch.py part2.html es4.css language/*.html ../builtins/*.es ../*.sml
	python stitch.py part2.html language.html

library.html: stitch.py part3.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part3.html library.html

clean:
	rm -f language.html language.pdf library.html library.pdf *~ language/*~ library/*~
