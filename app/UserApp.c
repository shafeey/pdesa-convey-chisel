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
  uint64_t  total_qconf, mem_conf, hist_conf, total_mem_delay;
  
  uint64_t  *cp_a0;
  
  uint64_t  sim_end_time = 1000;
  uint64_t  num_init_events = 64;
  uint64_t num_LP = 64;
  uint64_t num_mem_access = 0;
  uint64_t num_delays = 10;

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
	num_delays = atoi(argv[5]);
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
  
  uint64_t args[7];
  args[0] = (uint64_t) cp_a0; 
  args[1] = sim_end_time;
  args[2] = num_init_events;
  args[3] = num_mem_access;
  args[4] = core_mask;
  args[5] = num_LP;
  args[6] = num_delays;
  
    
  wdm_dispatch_t ds;
  memset((void *)&ds, 0, sizeof(ds));
  for (i=0; i<4; i++) {
    ds.ae[i].aeg_ptr_s = args;
    ds.ae[i].aeg_cnt_s = 7;
    ds.ae[i].aeg_base_s = 0;
    ds.ae[i].aeg_ptr_r = &report[i*16];
    ds.ae[i].aeg_cnt_r = 9;
    ds.ae[i].aeg_base_r = 7;
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
  total_stalls = report[2];
  total_events = report[3];
  total_antimsg = report[4];
  total_qconf = report[5];
  hist_conf = report[6];
  mem_conf = report[7];
  total_mem_delay = report[8];
    
  printf("Returned GVT = %lld\n", (long long) gvt);
  printf("Total cycles = %lld\n", (long long) total_cycles);
  printf("Total stall cycles = %lld\n", (long long) total_stalls);
  printf("Total events = %lld\n", (long long) total_events);
  printf("Total antimessages = %lld\n", (long long) total_antimsg);
  printf("Contention for queue = %lld\n", (long long) total_qconf);
  printf("Contention for history table = %lld\n", (long long) hist_conf);
  printf("Contention for memory interface = %lld\n", (long long) mem_conf);
  printf("Total memory access time per core = %lld\n", (long long) total_mem_delay);
  
  return 0;
}

// Print usage message and exit with error.
void
usage (char* p)
{
    printf("usage: %s [count (default 100)] \n", p);
    exit (1);
}


