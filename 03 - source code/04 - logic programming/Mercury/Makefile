#-----------------------------------------------------------------------------#
# vim: ts=8 sw=8 noexpandtab
#-----------------------------------------------------------------------------#

.PHONY: all

all: graph_4coloring_Australia

graph_4coloring_Australia: graph_4coloring_Australia.m eqneq.m
	mmc --make graph_4coloring_Australia

.PHONY: realclean
realclean:
	mmc --make graph_4coloring_Australia.realclean
	/bin/rm -rf Mercury
