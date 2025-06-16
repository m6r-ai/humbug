/*
 * wfb.c
 *	Simulation used for the hashingit.com article "Waiting For Blocks
 *	(Revised)" and the earlier "Waiting For Blocks".
 *
 * Copyright (C) 2015 David Hudson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define NEGATIVE_ORDERS 1
#define POSITIVE_ORDERS 10
#define NUM_BUCKETS_PER_ORDER 1000
#define NUM_BUCKETS (NUM_BUCKETS_PER_ORDER * (POSITIVE_ORDERS + NEGATIVE_ORDERS))

long int buckets1[NUM_BUCKETS];
int smallest_bucket1 = NUM_BUCKETS;
int largest_bucket1 = 0;
long long int num_results1 = 0LL;
long int buckets6[NUM_BUCKETS];
int smallest_bucket6 = NUM_BUCKETS;
int largest_bucket6 = 0;
long long int num_results6 = 0LL;

/*
 * sim_pp()
 *	Simulate one time period of a Poisson process.
 */
static double sim_pp(double rate)
{
	/*
	 * rand() isn't an ideal function here but if we go out of our way to fold in
	 * some entropy we're ok.
	 */
	double r = (double)rand() / (((double)RAND_MAX) + 1.0);

	return (double)(-log(1.0 - r) / rate);
}

/*
 * mine()
 *	Simulate a set of blocks being mined.
 */
static double mine(double init_hash_rate_ratio, double daily_hash_rate_ratio, FILE *f)
{
	double cumulative_time = 0.0;

	double last6[6];
	double last6_duration = 0.0;

	for (int i = 0; i < 2016; i++) {
		/*
		 * Randomize!
		 */
		unsigned int seed;
		if (fread(&seed, sizeof(seed), 1, f) != 1) {
			fclose(f);
			exit(-2);
		}

		srand(seed);

		double days = cumulative_time / (24.0 * 60.0 * 60.0);
		double hash_rate_ratio = init_hash_rate_ratio * pow(daily_hash_rate_ratio, days);

		/*
		 * Find the next block.
		 */
		double block_duration = sim_pp(hash_rate_ratio / 600.0);
		cumulative_time += block_duration;

		/*
		 * Bucketize the block duration.
		 */
		double log_block_duration = log10(block_duration);
		double log_block_duration_bucket = (double)NUM_BUCKETS_PER_ORDER * log_block_duration;
		long int b1 = (long int)ceil(log_block_duration_bucket);
		b1 += (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER);
		if (b1 < 0) {
			b1 = 0;
		}
		buckets1[b1]++;

		if (largest_bucket1 < b1) {
			largest_bucket1 = b1;
		}

		if (smallest_bucket1 > b1) {
			smallest_bucket1 = b1;
		}

		num_results1++;

		/*
		 * Track the last 6 block durations.  Note that we can't do anything until we
		 * have 6 blocks.
		 */
		last6_duration += block_duration;
		if (i >= 6) {
			last6_duration -= last6[0];

			/*
			 * Bucketize the last 6 blocks duration.
			 */
			double log_last6_duration = log10(last6_duration);
			double log_last6_duration_bucket = (double)NUM_BUCKETS_PER_ORDER * log_last6_duration;
			long int b6 = (long int)ceil(log_last6_duration_bucket);
			b6 += (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER);
			if (b6 < 0) {
				b6 = 0;
			}
			buckets6[b6]++;

			if (largest_bucket6 < b6) {
				largest_bucket6 = b6;
			}

			if (smallest_bucket6 > b6) {
				smallest_bucket6 = b6;
			}

			num_results6++;
		}

		last6[0] = last6[1];
		last6[1] = last6[2];
		last6[2] = last6[3];
		last6[3] = last6[4];
		last6[4] = last6[5];
		last6[5] = block_duration;
	}

	return cumulative_time;
}

/*
 * output_results()
 *	Generate the output results.
 */
static void output_results(void)
{
	double num_res1 = (double)num_results1;
	double cumulative_ratio1 = 0.0;
	printf("-- single blocks --\n");

	for (int i = smallest_bucket1; i <= largest_bucket1; i++) {
		double r = (double)buckets1[i] / num_res1;
		double bucket_start = pow(10.0, (double)(i - (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER)) / (double)NUM_BUCKETS_PER_ORDER); 
		double bucket_end = pow(10.0, (double)(i + 1 - (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER)) / (double)NUM_BUCKETS_PER_ORDER); 
		cumulative_ratio1 += r;
		printf("%d | %.6f | %.6f | %.6f | %.6f\n",
		       i, bucket_start, r, r / (bucket_end - bucket_start), cumulative_ratio1);
	}

	double num_res6 = (double)num_results6;
	double cumulative_ratio6 = 0.0;
	printf("\n-- six blocks --\n");

	for (int i = smallest_bucket6; i <= largest_bucket6; i++) {
		double r = (double)buckets6[i] / num_res6;
		double bucket_start = pow(10.0, (double)(i - (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER)) / (double)NUM_BUCKETS_PER_ORDER); 
		double bucket_end = pow(10.0, (double)(i + 1 - (NEGATIVE_ORDERS * NUM_BUCKETS_PER_ORDER)) / (double)NUM_BUCKETS_PER_ORDER); 
		cumulative_ratio6 += r;
		printf("%d | %.6f | %.6f | %.6f | %.6f\n",
		       i, bucket_start, r, r / (bucket_end - bucket_start), cumulative_ratio6);
	}
}

/*
 * sim()
 *	Simulate mining.
 */
void sim(double daily_hash_rate_ratio, int num_sims)
{
	/*
	 * We want some real randomness in our results.  Go and open a can of it!
	 */
	FILE *f = fopen("/dev/urandom", "rb");
	if (!f) {
		fprintf(stderr, "Failed to open /dev/urandom\n");
		return;
	}

	int divisor = num_sims / 100;
	if (divisor == 0) {
		divisor = 1;
	}

	/*
	 * We need to estimate our initial hash rate ratio based on the daily increment rate.
	 * This is tricky because the higher that rate the shorter the total period.  We need
	 * to seed this so we go with a default assumption.  Afterwards we use the results
	 * from the preceding simulation instead.
	 */
	double init_hash_rate_ratio = pow(daily_hash_rate_ratio, 7.0);

	/*
	 * Simulate many runs.
	 */
	for (int j = 0; j < num_sims; j++) {
		double duration = mine(init_hash_rate_ratio, daily_hash_rate_ratio, f);

		if ((j % divisor) == 0) {
			fprintf(stderr, "Sim: %d completed\n", j);
		}

		/*
		 * Calculate the next initial hash rate ratio.
		 */
		double days = duration / (60.0 * 60.0 * 24.0);
		init_hash_rate_ratio = pow(daily_hash_rate_ratio, (days / 2.0));
	}

	fclose(f);

	/*
	 * Produce output data.
	 */
	output_results();
}

int main(int argc, char **argv)
{
	if (argc != 3) {
		printf("usage: %s <daily-hash-rate-ratio> <num-sims>\n", argv[0]);
		exit(-1);
	}

	/*
	 * Number of blocks that we wish to model per simulation run.  If, say, this is 1008
	 * then this corresponds to a nominal week of mining as we're not modelling the
	 * network capacity expanding or contracting.
	 */
	double dhrr = atof(argv[1]);

	/*
	 * Number of simulation runs.  Larger is better here.  100k simulations should give
	 * pretty consistent results; 1M is better :-)
	 */
	int ns = atoi(argv[2]);

	sim(dhrr, ns);
}

