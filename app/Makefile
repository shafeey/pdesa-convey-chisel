EXEC = app

CFLAGS += -g -Wall
CFLAGS += -I/opt/convey/include

$(EXEC):  LIBS += -L /opt/convey/lib -lwx_runtime
$(EXEC)_sim:  LIBS += -L /opt/convey/lib -lwx_sim_runtime

all:  $(EXEC) $(EXEC)_sim

$(EXEC) $(EXEC)_sim: UserApp.c 
	$(CC) $(CFLAGS) UserApp.c $(LIBS) -o $@

clean:
	rm -f $(EXEC) $(EXEC)_sim runpdksim.log
