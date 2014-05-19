/** @file tlu-ctl.cpp
 *  @brief Command-line interface for TLU control registers
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2014 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Control registers and queue inspection.
 *
 *******************************************************************************
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library. If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************
 */

#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <unistd.h> /* getopt */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tlu.h"

using namespace GSI_TLU;

static const char* program;
static bool quiet;
static bool verbose;
static bool numeric;

static void help(void) {
  fprintf(stderr, "Usage: %s [OPTION] <etherbone-device> [command]\n", program);
  fprintf(stderr, "\n");
  fprintf(stderr, "  -a <address>              select a TLU unit by Wishbone address\n");
  fprintf(stderr, "  -t <tlu-id>               select a TLU unit by index #\n");
  fprintf(stderr, "  -c <channel-id>           select a trigger channel by index #\n");
  fprintf(stderr, "  -v                        verbose operation: print SDB records\n");
  fprintf(stderr, "  -q                        quiet: do not display table headers\n");
  fprintf(stderr, "  -n                        numeric dates\n");
  fprintf(stderr, "  -h                        display this help and exit\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  status                    report all top-level TLU information\n");
  fprintf(stderr, "  enable                    allow the TLU to send interrupts\n");
  fprintf(stderr, "  disable                   drop all interrupts from the TLU\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  pop                       pop the next pending timestamp from a channel\n");
  fprintf(stderr, "  pop_all                   pop all pending timestamps from a channel\n");
  fprintf(stderr, "  clear                     flush the contents of the TLU channel\n");
  fprintf(stderr, "  test                      generate a virtual event timestamp\n");
  fprintf(stderr, "  ignore                    stop channel from recording edges\n");
  fprintf(stderr, "  listen <pos|neg> [stable] set channel to record pos/neg edges\n");
  fprintf(stderr, "  unhook                    disable arrival interrupts from this channel\n");
  fprintf(stderr, "  hook <addr> [msg]         enable arrival interrupts on this channel\n");
}

static void die(eb_status_t status, const char* what) {
  fprintf(stderr, "%s: %s -- %s\n", program, what, eb_status(status));
  exit(1);
}

static void render_time(uint64_t time) {
  if (numeric) {
    printf("time:0x%"PRIx64"", time);
  } else {
    #define BILLION 1000000000ULL
    time_t now = time / BILLION;
    double fraction = time % BILLION;
    fraction /= BILLION;
    
    struct tm *tm = gmtime(&now);
    char buf[40];
    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", tm);
    
    printf("%s.%.9f", buf, fraction);
  }
}

static void render_tlu(int i, TLU& tlu) {
  printf("TLU #%d (0x%"EB_ADDR_FMT") with %d-deep channels and interrupts %s\n",
   i, tlu.address, tlu.queue_size, tlu.int_enable?"enabled":"disabled");
  printf("  "); render_time(tlu.current_time); printf("\n");

  
  for (unsigned c = 0; c < tlu.channels.size(); ++c) {
    printf("  Channel #%d %s %s: %3d queued, %3d stable time, int:",
      c, tlu.channels[c].active?"listen":"ignore",
      tlu.channels[c].active?(tlu.channels[c].pos_edge?"pos":"neg"):"   ",
      tlu.channels[c].queued, tlu.channels[c].stable);
    if (tlu.channels[c].int_enable) {
      printf("0x%"PRIx32",0x%"PRIx32"\n", 
        tlu.channels[c].int_dest, 
        tlu.channels[c].int_msg);
    } else {
      printf("disabled\n");
    }
  }
}

int main(int argc, char** argv) {
  int opt, error;
  char *value_end;
  const char *devpath, *command;
  int tlu_id = -1, channel_id = -1;
  eb_address_t tlu_addr = 0;
  bool tlu_addr_set = false;
  eb_status_t status;
  
  program = argv[0];
  error = 0;
  
  while ((opt = getopt(argc, argv, "a:t:c:vqnh")) != -1) {
    switch (opt) {
    case 'a':
      tlu_addr = strtoull(optarg, &value_end, 0);
      if (*value_end != 0) {
        fprintf(stderr, "%s: invalid TLU address -- '%s'\n", program, optarg);
        error = 1;
      } else {
        tlu_addr_set = true;
      }
      break;
    case 't':
      tlu_id = strtol(optarg, &value_end, 0);
      if (*value_end || tlu_id < 0 || tlu_id > 100) {
        fprintf(stderr, "%s: invalid TLU id -- '%s'\n", program, optarg);
        error = 1;
      }
      break;       
    case 'c':
      channel_id = strtol(optarg, &value_end, 0);
      if (*value_end || channel_id < 0 || channel_id > 32) {
        fprintf(stderr, "%s: invalid channel id -- '%s'\n", program, optarg);
        error = 1;
      }
      break;
    case 'v':
      verbose = true;
      break;
    case 'q':
      quiet = true;
      break;
    case 'n':
      numeric = true;
      break;
    case 'h':
      help();
      return 0;
    case ':':
    case '?':
      error = 1;
      break;
    default:
      fprintf(stderr, "%s: bad getopt result\n", program);
      return 1;
    }
  }
  
  if (error) return 1;
  
  if (optind >= argc) {
    fprintf(stderr, "%s: expecting one non-optional argument: <etherbone-device>\n", program);
    fprintf(stderr, "\n");
    help();
    return 1;
  }
  
  devpath = argv[optind];
  
  if (optind+1 < argc) {
    command = argv[optind+1];
  } else {
    command = "status";
  }
  
  if (strcasecmp(command, "hook") == 0) {
    if (optind+3 > argc) {
      fprintf(stderr, "%s: expecting exactly one-two arguments: hook <addr> [msg]\n", program);
      return 1;
    }
    if (optind+4 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+4]);
      return 1;
    }
  } else if (strcasecmp(command, "listen") == 0) {
    if (optind+3 > argc) {
      fprintf(stderr, "%s: expecting exactly one-two arguments: listen <pos|neg> [stable]\n", program);
      return 1;
    }
    if (optind+4 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+4]);
      return 1;
    }
    if (strcasecmp(argv[optind+2], "pos") != 0 &&
        strcasecmp(argv[optind+2], "neg") != 0) {
      fprintf(stderr, "%s: listen expects <pos|neg>, not '%s'\n", program, argv[optind+2]);
      return 1;
    }
  } else {
    if (optind+2 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
  }
  
  if (tlu_addr_set && tlu_id != -1) {
    fprintf(stderr, "%s: cannot set both -a and -t at once.\n", program);
    return 1;
  }
  
  Socket socket;
  if ((status = socket.open()) != EB_OK) die(status, "etherbone::socket.open");
  
  Device device;
  if ((status = device.open(socket, devpath)) != EB_OK) {
    fprintf(stderr, "%s: etherbone::device.open('%s') -- %s\n", program, devpath, eb_status(status));
    return 1;
  }
  
  std::vector<TLU> tlus;
  if ((status = TLU::probe(device, tlus)) != EB_OK) die(status, "TLU::probe");
  
  if (tlus.empty()) {
    fprintf(stderr, "%s: no TLU units found\n", program);
    return 1;
  }
  
  if (tlu_addr_set) {
    unsigned i;
    for (i = 0; i < tlus.size(); ++i) {
      if (tlus[i].address == tlu_addr) break;
    }
    if (i == tlus.size()) {
      fprintf(stderr, "%s: no TLU found at address 0x%"EB_ADDR_FMT"\n", program, tlu_addr);
      return 1;
    }
    tlu_id = (int)i;
  }
  
  /* Select default TLU unit */
  if (tlu_id == -1 && tlus.size() == 1) {
    tlu_id = 0;
  }
  
  if (channel_id != -1 && tlu_id == -1) {
    fprintf(stderr, "%s: cannot specify a channel id -c '%d' without specifying a TLU unit with -a/-t\n",
      program, channel_id);
    return 1;
  }
  
  if (tlu_id >= (int)tlus.size()) {
    fprintf(stderr, "%s: TLU id '%d' is out of range; max=%d\n", program, tlu_id, (int)tlus.size()-1);
    return 1;
  }
  
  /* Pick a default channel */
  if (channel_id == -1 && tlu_id != -1 && tlus[tlu_id].channels.size() == 1) {
    channel_id = 0;
  }
  
  if (channel_id != -1 && channel_id >= (int)tlus[tlu_id].channels.size()) {
    fprintf(stderr, "%s: channel id -c '%d' is out of range; max=%d\n", program, channel_id, (int)tlus[tlu_id].channels.size()-1);
    return 1;
  }
  
  /* -------------------------------------------------------------------- */
  if (!strcasecmp(command, "status")) {
    if (tlu_id == -1) {
      for (unsigned i = 0; i < tlus.size(); ++i) {
        if (i > 0) printf("\n");
        render_tlu(i, tlus[i]);
      }
    } else {
      render_tlu(tlu_id, tlus[tlu_id]);
    }
  }

  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "enable")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU unit to enable with -t/-a\n", program);
      return 1;
    }
    
    if (verbose) {
      printf("Enabling TLU #%d (0x%"EB_ADDR_FMT")\n", tlu_id, tlus[tlu_id].address);
    }
    
    if ((status = tlus[tlu_id].set_enable(true)) != EB_OK)
      die(status, "TLU::set_enable(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "disable")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU unit to disable with -t/-a\n", program);
      return 1;
    }
    
    if (verbose) {
      printf("Disabling TLU #%d (0x%"EB_ADDR_FMT")\n", tlu_id, tlus[tlu_id].address);
    }
    
    if ((status = tlus[tlu_id].set_enable(false)) != EB_OK)
      die(status, "TLU::set_enable(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "pop")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to pop with -c\n", program);
      return 1;
    }
    if (tlus[tlu_id].channels[channel_id].queued == 0) {
      fprintf(stderr, "%s: channel is empty\n", program);
      return 1;
    }
    if (verbose) {
      printf("Popping channel #%d on TLU #%d (0x%"EB_ADDR_FMT")\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    uint64_t time;
    if ((status = tlus[tlu_id].pop(channel_id, time)) != EB_OK)
      die(status, "TLU::pop");
    render_time(time); 
    printf("\n");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "pop_all")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU to pop_all with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Popping all from channel #%d on TLU #%d (0x%"EB_ADDR_FMT")\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    
    if (channel_id == -1) {
      std::vector<std::vector<uint64_t> > queues;
      if ((status = tlus[tlu_id].pop_all(queues)) != EB_OK)
        die(status, "TLU::pop_all");
      for (unsigned c = 0; c < queues.size(); ++c) {
        std::vector<uint64_t>& queue = queues[c];
        printf("Channel #%d:\n", c);
        for (unsigned e = 0; e < queue.size(); ++e) {
          printf("  ");
          render_time(queue[e]);
          printf("\n");
        }
      }
    } else {
      std::vector<uint64_t> queue;
      if ((status = tlus[tlu_id].pop_all(channel_id, queue)) != EB_OK)
        die(status, "TLU::pop_all");
      for (unsigned e = 0; e < queue.size(); ++e) {
        render_time(queue[e]);
        printf("\n");
      }
    }
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "clear")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU to clear with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Clearing channel #%d on TLU #%d (0x%"EB_ADDR_FMT")\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    if ((status = tlus[tlu_id].clear(channel_id)) != EB_OK)
      die(status, "TLU::clear");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "listen")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU to listen with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Setting channel #%d on TLU #%d (0x%"EB_ADDR_FMT") to watch for %s edges\n",
        channel_id, tlu_id, tlus[tlu_id].address, argv[optind+2]);
    }
    bool pos = strcasecmp(argv[optind+2], "pos") == 0;
    int stable = 8;
    if (argc>optind+3) {
      stable = strtoul(argv[optind+3], &value_end, 0);
      if (*value_end != 0 || stable < 8 || stable > 1000000) {
        fprintf(stderr, "%s: stable time invalid -- '%s'\n", program, argv[optind+3]);
        return 1;
      }
    }
    if ((status = tlus[tlu_id].listen(channel_id, true, pos, stable)) != EB_OK)
      die(status, "TLU::listen(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "ignore")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU to ignore with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Setting channel #%d on TLU #%d (0x%"EB_ADDR_FMT") to ignore edges\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    if ((status = tlus[tlu_id].listen(channel_id, false, false)) != EB_OK)
      die(status, "TLU::listen(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "unhook")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU unhook with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Unhooking channel #%d on TLU #%d (0x%"EB_ADDR_FMT")\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    if ((status = tlus[tlu_id].hook(channel_id, false)) != EB_OK)
      die(status, "TLU::hook(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "hook")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU hook with -t/-a\n", program);
      return 1;
    }
    uint32_t dest, msg;
    dest = strtoul(argv[optind+2], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid interrupt destination -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
    msg = (argc>optind+3)?strtoul(argv[optind+3], &value_end, 0):0;
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid interrupt message -- '%s'\n", program, argv[optind+3]);
      return 1;
    }
    if (verbose) {
      printf("Hooking channel #%d on TLU #%d (0x%"EB_ADDR_FMT") to 0x%"PRIx32", 0x%"PRIx32"\n",
        channel_id, tlu_id, tlus[tlu_id].address, dest, msg);
    }
    if ((status = tlus[tlu_id].hook(channel_id, true, dest, msg)) != EB_OK)
      die(status, "TLU::hook(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "test")) {
    if (tlu_id == -1) {
      fprintf(stderr, "%s: specify a TLU to test with -t/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Testing channel #%d on TLU #%d (0x%"EB_ADDR_FMT")\n",
        channel_id, tlu_id, tlus[tlu_id].address);
    }
    if ((status = tlus[tlu_id].test(channel_id, 0xff)) != EB_OK)
      die(status, "TLU::test");
  }
  
  /* -------------------------------------------------------------------- */
  else {
    fprintf(stderr, "%s: unknown command -- '%s'\n", program, command);
    return 1;
  }
  
  return 0;
}
