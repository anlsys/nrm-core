/*******************************************************************************
 * Copyright 2019 UChicago Argonne, LLC.
 * (c.f. AUTHORS, LICENSE)
 *
 * SPDX-License-Identifier: BSD-3-Clause
*******************************************************************************
 *
 *    this file is generated, modifications will be erased.
*/
#define NRM_CMDPERFORMANCE_FORMAT "{\"tag\":\"cmdperformance\",\"cmdID\": \"%s\",\"timestamp\": \"%f\",\"perf\": \"%d\"}"
#define NRM_CMDPAUSE_FORMAT "{\"tag\":\"cmdpause\",\"cmdID\": \"%s\",\"timestamp\": \"%f\"}"
#define NRM_THREADPROGRESS_FORMAT "{\"tag\":\"threadprogress\",\"cmdID\": \"%s\",\"processID\": \"%d\",\"taskID\": \"%s\",\"threadID\": \"%d\",\"payload\": \"%d\"}"
#define NRM_THREADPAUSE_FORMAT "{\"tag\":\"threadpause\",\"cmdID\": \"%s\",\"processID\": \"%d\",\"taskID\": \"%s\",\"threadID\": \"%d\"}"
#define NRM_THREADPHASECONTEXT_FORMAT "{\"tag\":\"threadphasecontext\",\"cmdID\": \"%s\",\"processID\": \"%d\",\"taskID\": \"%s\",\"threadID\": \"%d\",\"cpu\": \"%d\",\"aggregation\": \"%d\",\"computetime\": \"%d\",\"totaltime\": \"%d\"}"
#define NRM_THREADPHASEPAUSE_FORMAT "{\"tag\":\"threadphasepause\",\"cmdID\": \"%s\",\"processID\": \"%d\",\"taskID\": \"%s\",\"threadID\": \"%d\"}"