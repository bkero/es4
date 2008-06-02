dummy: language.html ##library.html 

language.html: stitch.py part2.html es4.css language/*.html ../builtins/*.es ../*.sml
	python stitch.py part2.html language.html

library.html: stitch.py part3.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part3.html library.html

appendicies.html: stitch.py part4.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py part4.html appendicies.html

lib-march-2008.html: stitch.py lib-march-2008-template.html es4.css library/*.html ../builtins/*.es ../*.sml
	python stitch.py lib-march-2008-template.html lib-march-2008.html

LIBDEPS=stitch.py es4.css ../builtins/*.es ../*.sml
LANGDEPS=stitch.py es4.css grammar.xls ../*.sml

output/Expressions.html: Expressions.html language/exprs.html language/exprs2.html $(LANGDEPS) 
	perl mergeGrammar.pl language/exprs.html language/exprs2.html
	python stitch.py Expressions.html output/Expressions.html

output/Global.html: Global.html library/global.html $(LIBDEPS) 
	python stitch.py Global.html output/Global.html

output/Object.html: Object.html library/Object.html $(LIBDEPS) 
	python stitch.py Object.html output/Object.html

output/Date.html: Date.html library/Date.html $(LIBDEPS) 
	python stitch.py Date.html output/Date.html

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

output/enumerability.html: enumerability.html language/enumerability.html $(LIBDEPS)
	python stitch.py enumerability.html output/enumerability.html

output/readwrite.html: readwrite.html language/readwrite.html $(LIBDEPS)
	python stitch.py readwrite.html output/readwrite.html

output/names.html: names.html language/names.html $(LIBDEPS)
	python stitch.py names.html output/names.html

output/assert.html: assert.html language/assert.html $(LIBDEPS)
	python stitch.py assert.html output/assert.html

output/strict-mode.html: strict-mode.html language/strict-mode.html $(LIBDEPS)
	python stitch.py strict-mode.html output/strict-mode.html

output/object-literals.html: object-literals.html language/object-literals.html $(LIBDEPS)
	python stitch.py object-literals.html output/object-literals.html

output/definitions.html: definitions.html language/definitions.html $(LIBDEPS)
	python stitch.py definitions.html output/definitions.html

output/array-literals.html: array-literals.html language/array-literals.html $(LIBDEPS)
	python stitch.py array-literals.html output/array-literals.html

output/meta-issues.html: meta-issues.html library/meta-issues.html $(LIBDEPS)
	python stitch.py meta-issues.html output/meta-issues.html

typesystem.html: stitch.py parttypes.html es4.css typesystem/*.html ../builtins/*.es ../*.sml
	python stitch.py parttypes.html typesystem.html

clean:
	rm -f language.html language.pdf library.html library.pdf *~ language/*~ library/*~
