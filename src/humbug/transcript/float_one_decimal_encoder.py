"""Transcript writer implementation for Humbug application."""

import asyncio
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
import json
import os
import shutil
import sys
from typing import List, Dict, Optional


class FloatOneDecimalEncoder(json.JSONEncoder):
    """JSON encoder that formats floats to one decimal place."""
    def default(self, obj):
        if isinstance(obj, float):
            return round(obj, 1)
        return super().default(obj)
