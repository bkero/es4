dummy: language.html library.html

language.html: stitch.py part2.html es4.css language/*.html ../builtins/*.es ../*.sml
	python stitch.py part2.html language.html

library.html: stitch.py part3.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part3.html library.html

LIBDEPS=stitch.py es4.css ../builtins/*.es ../*.sml

output/Vector.html: Vector.html library/Vector.html $(LIBDEPS) 
	python stitch.py Vector.html output/Vector.html

output/Map.html: Map.html library/Map.html $(LIBDEPS)
	python stitch.py Map.html output/Map.html

typesystem.html: stitch.py parttypes.html es4.css typesystem/*.html ../builtins/*.es ../*.sml
	python stitch.py parttypes.html typesystem.html

clean:
	rm -f language.html language.pdf library.html library.pdf *~ language/*~ library/*~
