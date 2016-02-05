#
# Mettre la liste des fichiers .ml, .mly, .mll et .mli
# constituant le projet. Si un fichier b.ml dépend d'un fichier
# a.ml, a.ml doit se trouver avant dans la liste.
#

SOURCES = mips.mli mips.ml ast.mli error.ml interpreter.ml \
	  parser.mly lexer.mll compile.ml main.ml

# Répertoires contenant les tests

TEST_DIRECTORIES = print arith
TEST_DIRECTORIES_INTERPRET = print arith

# Nom du binaire

EXEC = compilo

# Nom de l'utilitaire MARS

MARS_EXEC = ../Mars4_5.jar

#######################################################################
# Partie générique, ne pas modifier.
#######################################################################




# Compilateurs

CAMLC = ocamlc -g -annot
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# Interprète

INTERPRET = ./$(EXEC) -i
TYPE_AND_INTERPRET = ./$(EXEC) -ti

# Sources

SRC_MLL = $(filter %.mll, $(SOURCES))
SRC_MLY = $(filter %.mly, $(SOURCES))
SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

# Tests

MARS = java -jar $(MARS_EXEC)

TEST_SUBDIRECTORY = $(DIR)
TEST_SRC = $(shell ls tests/$(TEST_SUBDIRECTORY)/*.ml)
TEST_ASM = $(TEST_SRC:.ml=.asm)

TEST_SRC_ALL = $(foreach test_dir, $(TEST_DIRECTORIES), \
	         $(shell ls tests/$(test_dir)/*.ml))
TEST_ASM_ALL = $(TEST_SRC_ALL:.ml=.asm)

TEST_I_SRC_ALL = $(foreach test_dir, $(TEST_DIRECTORIES_INTERPRET), \
	         $(shell ls tests/$(test_dir)/*.ml))


# Cibles

all: depend $(EXEC)

opt: depend $(EXEC).opt


.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .asm

$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)


.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

.ml.asm: 
	./$(EXEC) $<

test: all $(TEST_ASM)
	@$(foreach file, $(TEST_ASM), \
	echo "$(file)"; $(MARS) $(file); echo "------ ------ ------ ------\n";)

test_all: all $(TEST_ASM_ALL)
	@$(foreach file, $(TEST_ASM_ALL), \
	echo "$(file)"; $(MARS) $(file); echo "------ ------ ------ ------\n";)

test_i:
	@$(foreach file, $(TEST_SRC), \
	echo "$(file)"; $(INTERPRET) $(file); echo "------ ------ ------ ------\n";)

test_i_all:
	@$(foreach file, $(TEST_I_SRC_ALL), \
	echo "$(file)"; $(INTERPRET) $(file); echo "------ ------ ------ ------\n";)

clean:
	rm -f *.cm[iox] *~ .*~ *.o *.annot
	rm -f $(SRC_MLL:.mll=.ml) $(SRC_MLY:.mly=.ml) $(SRC_MLY:.mly=.mli)
	rm -f $(TEST_ASM_ALL)
	rm -f $(EXEC)
	rm -f $(EXEC).opt

clean_test_all:
	rm -f $(TEST_ASM_ALL)

clean_test:
	rm -f $(TEST_ASM)


depend:
	$(CAMLDEP) $(SMLIY) > .depend

-include .depend
