DEBUG = 1

include config.make

OCAML_LIB_OBJ  = nano \
                 nanoLex \
                 nanoParse 
OCAML_EXE_OBJ  = $(OCAML_LIB_OBJ) main
OCAML_EXE      = nanoml

MODULES        = $(OCAML_EXE_OBJ) 
MLYS           = nanoParse.mly 
MLLS           = nanoLex.mll 

#TODO           = $(OCAML_EXE).top $(OCAML_EXE).byte $(OCAML_EXE).opt
TODO           = $(OCAML_EXE).top $(OCAML_EXE).byte

all:: $(TODO)

clean: default-clean

distclean: clean
	rm -f *.cmi *.annot $(TODO) $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) \
	$(MLLS:%.mll=%.ml)

.SECONDARY: $(MLYS:%.mly=%.mli) $(MLYS:%.mly=%.ml) $(MLLS:%.mll=%.ml)

.PHONY : all clean distclean

# Implicit rules

include rules.make

# Dependencies

-include $(MODULES:%=.%.ml.depend) $(MODULES:%=.%.mli.depend)
