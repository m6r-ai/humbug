"""Implements a sliding window rate limiter."""

import asyncio
from collections import deque
import time


class AIRateLimiter:
    """Implements a sliding window rate limiter."""

    def __init__(self, window_size: int = 60, max_requests: int = 50):
        """Initialize rate limiter with window size in seconds."""
        self._window_size = window_size
        self._max_requests = max_requests
        self._requests = deque()

    async def acquire(self):
        """Wait until a request can be made within rate limits."""
        now = time.time()

        # Remove expired timestamps
        while self._requests and self._requests[0] <= now - self._window_size:
            self._requests.popleft()

        if len(self._requests) >= self._max_requests:
            # Calculate sleep time
            sleep_time = self._requests[0] + self._window_size - now
            if sleep_time > 0:
                await asyncio.sleep(sleep_time)

        self._requests.append(now)
