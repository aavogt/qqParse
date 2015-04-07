
Main.hshtml: QQParse.tix
	hpc markup QQParse.tix


QQParse.tix: QQParse
	rm QQParse.tix
	./QQParse

QQParse: QQParse.hs UULayout.hs
	ghc -fhpc QQParse.hs


clean:
	rm -f hpc_index_alt.html  hpc_index_exp.html  hpc_index_fun.html  hpc_index.html  Main.hs.html  QQParse  QQParse.hi  QQParse.o  QQParse.tix QQParse.prof UULayout.hi UULayout.hs.html UULayout.o
