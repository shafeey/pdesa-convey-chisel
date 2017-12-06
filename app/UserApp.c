#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wdm_user.h>
#include <errno.h>

#undef DEBUG

extern long cpPhold();
void usage (char *);

int main(int argc, char *argv[])
{
  long i;
  uint64_t  gvt;
  uint64_t  total_cycles;
  uint64_t  total_events;
  uint64_t  total_stalls;
  uint64_t  total_antimsg;
  uint64_t  total_qconf, rcv_qconf, send_qconf, mem_conf, hist_conf;
  uint64_t  avg_mem_time;
  uint64_t  avg_proc_time;
  uint64_t  avg_hist_time;
  
  uint64_t  *cp_a0;
  
  uint64_t  sim_end_time = 1000;
  uint64_t  num_init_events = 64;
  uint64_t num_LP = 64;
  uint64_t num_mem_access = 0;
  uint64_t num_delay = 10;

  uint64_t num_cores = 64;
  uint64_t core_mask = 0xFFFFFFFFFFFFFFFF; // 64 cores
//  uint64_t core_mask = 0x00000000FFFFFFFF; // 32 cores
//  uint64_t core_mask = 0x000000000000FFFF; // 16 cores
//  uint64_t core_mask = 0x00000000000000FF; // 08 cores
//  uint64_t core_mask = 0x000000000000000F; // 04 cores
//  uint64_t core_mask = 0x0000000000000003; // 02 cores
//  uint64_t core_mask = 0x0000000000000001; // 01 cores
  
  uint64_t report[64];
  long size = 8;

  // check command line args
  if (argc == 1) {
    sim_end_time = 100;		// default size
  } else if (argc == 2) {
    sim_end_time = atoi(argv[1]);
    if (sim_end_time == 0) {
      usage (argv[0]);
      return 0;
    }
  }
  else if (argc == 5){
	sim_end_time = atoi(argv[1]);
	num_LP = atoi(argv[2]);
    num_init_events = atoi(argv[3]);
	num_mem_access = atoi(argv[4]);
  }
  else if (argc == 6){
	sim_end_time = atoi(argv[1]);
	num_LP = atoi(argv[2]);
    num_init_events = atoi(argv[3]);
	num_mem_access = atoi(argv[4]);
	num_delay = atoi(argv[5]);
  }
  else {
    usage (argv[0]);
    return 0;
  }
  
  printf("Simulation will run until GVT = %lld with %lld LPs and %lld initial events on %lld cores\n",
			(long long) sim_end_time, (long long) num_LP, (long long) num_init_events, (long long) num_cores);
  fflush(stdout);

  
  // Reserve and attach to the coprocessor
  // The "pdk" personality is the PDK sample vadd personality
  wdm_coproc_t m_coproc = WDM_INVALID;
  m_coproc = wdm_reserve(WDM_CPID_ANY, NULL);

  if (m_coproc == WDM_INVALID) {
      fprintf(stderr, "Unable to reserve coprocessor\n");
      return -1;
  }

  char *name = "pdk";
  if (wdm_attach(m_coproc, name)) {
      fprintf(stderr, "Unable to attach signature \"%s\"\n", name);
      fprintf(stderr, " Please verify that the personality is installed in");
      fprintf(stderr, " /opt/convey/personalities or CNY_PERSONALITY_PATH is set.\n");
      return -1;
  }

  // Allocate memory on coprocessor
  wdm_posix_memalign(m_coproc, (void**)&cp_a0, 64, size*128);
  printf("Address passed to CAE: %p\n", cp_a0);
    
  num_LP = num_LP-1;
  
  uint64_t args[5];
  args[0] = (uint64_t) cp_a0; 
  args[1] = sim_end_time;
  args[2] = num_init_events; 
  args[3] = num_LP | (num_mem_access << 16) | (num_delay << 32);
  args[4] = core_mask;
    
  wdm_dispatch_t ds;
  memset((void *)&ds, 0, sizeof(ds));
  for (i=0; i<4; i++) {
    ds.ae[i].aeg_ptr_s = args;
    ds.ae[i].aeg_cnt_s = 5;
    ds.ae[i].aeg_base_s = 0;
    ds.ae[i].aeg_ptr_r = &report[i*16];
    ds.ae[i].aeg_cnt_r = 10;
    ds.ae[i].aeg_base_r = 5;
  }

  if (wdm_dispatch(m_coproc, &ds)) {
    perror("dispatch error");
    exit(-1);
  }

  int stat = 0;
  while (!(stat = wdm_dispatch_status(m_coproc)))
      usleep(10000);

  if (stat < 0) {
    perror("dispatch status error");
    exit(-1);
  }

  gvt = report[0];
  total_cycles = report[1];
  total_events = report[2];
  total_stalls = report[3];
  total_antimsg = report[4];
  total_qconf = (report[5] >> 42) << 3;
  rcv_qconf = ((report[5] >> 21) & 0x1FFFFF ) << 3;
  send_qconf = ((report[5]) & 0x1FFFFF ) << 3;
  
  avg_proc_time = report[6];
  avg_mem_time = report[7];
  avg_hist_time = report[8];
  
  mem_conf = (report[9] >> 32);
  hist_conf = (report[9] & 0xFFFFFFFF);
    

  printf("Returned GVT = %lld\n", (long long) gvt);
  printf("Total cycles = %lld\n", (long long) total_cycles);
  printf("Total events = %lld\n", (long long) total_events);
  printf("Total antimessages = %lld\n", (long long) total_antimsg);
  printf("Total stall cycles = %lld\n", (long long) total_stalls);
  printf("Total active time per core = %lld\n", (long long) avg_proc_time);
  printf("Total memory access time per core = %lld\n", (long long) avg_mem_time);
  printf("Total history access time per core = %lld\n", (long long) avg_hist_time);
  printf("Contention for queue = %lld\n", (long long) total_qconf);
  printf("Contention for queue receive = %lld\n", (long long) rcv_qconf);
  printf("Contention for queue send= %lld\n", (long long) send_qconf);
  printf("Contention for history table = %lld\n", (long long) hist_conf);
  printf("Contention for memory interface = %lld\n", (long long) mem_conf);
  
  return 0;
}

// Print usage message and exit with error.
void
usage (char* p)
{
    printf("usage: %s [count (default 100)] \n", p);
    exit (1);
}


