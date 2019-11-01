/*******************************************************************************
 * Copyright 2019 UChicago Argonne, LLC.
 * (c.f. AUTHORS, LICENSE)
 *
 * SPDX-License-Identifier: BSD-3-Clause
*******************************************************************************
 *
 *    this file is generated, modifications will be erased.
*/
#define NRM_CMDPERFORMANCE_FORMAT "{\"cmdPerformance\":{\"cmdID\": \"%s\",\"perf\": %d}}"
#define NRM_CMDPAUSE_FORMAT "{\"cmdPause\":{\"cmdID\": \"%s\"}}"
#define NRM_THREADPROGRESS_FORMAT "{\"threadProgress\":{\"cmdID\": \"%s\",\"processID\": %d,\"taskID\": \"%s\",\"threadID\": %d,\"rankID\": %d,\"payload\": %d}}"
#define NRM_THREADPAUSE_FORMAT "{\"threadPause\":{\"cmdID\": \"%s\",\"processID\": %d,\"taskID\": \"%s\",\"threadID\": %d,\"rankID\": %d}}"
#define NRM_THREADPHASECONTEXT_FORMAT "{\"threadPhaseContext\":{\"cmdID\": \"%s\",\"processID\": %d,\"taskID\": \"%s\",\"threadID\": %d,\"rankID\": %d,\"cpu\": %d,\"aggregation\": %d,\"computetime\": %d,\"totaltime\": %d}}"
#define NRM_THREADPHASEPAUSE_FORMAT "{\"threadPhasePause\":{\"cmdID\": \"%s\",\"processID\": %d,\"taskID\": \"%s\",\"threadID\": %d,\"rankID\": %d}}"