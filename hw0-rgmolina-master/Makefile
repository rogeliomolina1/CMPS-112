
#####################################################################################################
COURSE=CMPS 112
ASGN=HW0
COMPILER=rgmolina
#####################################################################################################

test:
	stack test

bin:
	stack build

clean:
	stack clean

prepare: clean
	tar -zcvf ../$(ASGN)-$(COMPILER).tgz --exclude .git --exclude .stack-work ../$(ASGN)-$(COMPILER)
	mv ../$(ASGN)-$(COMPILER).tgz .
