FLAGS = -g -Wall -Wpedantic -Werror
TARGET = jlisp.c

all:
	$(CC) $(FLAGS) $(TARGET) -o jlisp

.PHONY: all
