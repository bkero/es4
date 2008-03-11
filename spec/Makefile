dummy: language.html library.html

language.html: stitch.py part2.html es4.css language/*.html ../builtins/*.es ../*.sml
	python stitch.py part2.html language.html

library.html: stitch.py part3.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part3.html library.html

LIBDEPS=stitch.py es4.css ../builtins/*.es ../*.sml

output/Object.html: Object.html library/Object.html $(LIBDEPS) 
	python stitch.py Object.html output/Object.html

output/Function.html: Function.html library/Function.html $(LIBDEPS) 
	python stitch.py Function.html output/Function.html

output/Name.html: Name.html library/Name.html $(LIBDEPS) 
	python stitch.py Name.html output/Name.html

output/Namespace.html: Namespace.html library/Namespace.html $(LIBDEPS) 
	python stitch.py Namespace.html output/Namespace.html

output/Vector.html: Vector.html library/Vector.html $(LIBDEPS) 
	python stitch.py Vector.html output/Vector.html

output/Map.html: Map.html library/Map.html $(LIBDEPS)
	python stitch.py Map.html output/Map.html

output/Error.html: Error.html library/Error.html $(LIBDEPS)
	python stitch.py Error.html output/Error.html

typesystem.html: stitch.py parttypes.html es4.css typesystem/*.html ../builtins/*.es ../*.sml
	python stitch.py parttypes.html typesystem.html

clean:
	rm -f language.html language.pdf library.html library.pdf *~ language/*~ library/*~
