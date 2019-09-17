/*******************************************************************************
 * Copyright 2019 UChicago Argonne, LLC.
 * (c.f. AUTHORS, LICENSE)
 *
 * SPDX-License-Identifier: BSD-3-Clause
*******************************************************************************/
 *
 *    this file is generated, modifications will be erased.
*/
#define NRM_CMDSTART_FORMAT "{\"tag\":\"cmdstart\",\"cmdID\": \"%s\"}"
#define NRM_CMDPERFORMANCE_FORMAT "{\"tag\":\"cmdperformance\",\"cmdID\": \"%s\",\"perf\": \"%d\"}"
#define NRM_CMDEXIT_FORMAT "{\"tag\":\"cmdexit\",\"cmdID\": \"%s\"}"
#define NRM_THREADSTART_FORMAT "{\"tag\":\"threadstart\",\"cmdID\": \"%s\",\"processID\": \"%s\",\"taskID\": \"%s\",\"threadID\": \"%s\"}"
#define NRM_THREADPROGRESS_FORMAT "{\"tag\":\"threadprogress\",\"cmdID\": \"%s\",\"processID\": \"%s\",\"taskID\": \"%s\",\"threadID\": \"%s\",\"payload\": \"%d\"}"
#define NRM_THREADPHASECONTEXT_FORMAT "{\"tag\":\"threadphasecontext\",\"cmdID\": \"%s\",\"processID\": \"%s\",\"taskID\": \"%s\",\"threadID\": \"%s\",\"cpu\": \"%d\",\"startcompute\": \"%d\",\"endcompute\": \"%d\",\"startbarrier\": \"%d\",\"endbarrier\": \"%d\"}"
#define NRM_THREADEXIT_FORMAT "{\"tag\":\"threadexit\",\"cmdID\": \"%s\",\"processID\": \"%s\",\"taskID\": \"%s\",\"threadID\": \"%s\"}"