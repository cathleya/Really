src/really_handler.erl:: src/data_handler.erl src/talk_handler.erl; @touch $@
src/rest_handler.erl:: src/data_handler.erl; @touch $@

COMPILE_FIRST += data_handler talk_handler
