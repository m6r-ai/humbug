"""Implements a sliding window rate limiter."""

import asyncio
from collections import deque
import time


class RateLimiter:
    """Implements a sliding window rate limiter."""

    def __init__(self, window_size: int = 60, max_requests: int = 50):
        """Initialize rate limiter with window size in seconds."""
        self.window_size = window_size
        self.max_requests = max_requests
        self.requests = deque()

    async def acquire(self):
        """Wait until a request can be made within rate limits."""
        now = time.time()

        # Remove expired timestamps
        while self.requests and self.requests[0] <= now - self.window_size:
            self.requests.popleft()

        if len(self.requests) >= self.max_requests:
            # Calculate sleep time
            sleep_time = self.requests[0] + self.window_size - now
            if sleep_time > 0:
                await asyncio.sleep(sleep_time)

        self.requests.append(now)
