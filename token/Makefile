default:
	clear
	flex -l test.l
	bison -dv test.y 
	mkdir -p compiled
	gcc -o compiled/test test.tab.c lex.yy.c -lfl
