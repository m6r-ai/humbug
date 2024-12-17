"""Transcript writer implementation for Humbug application."""

import json


class FloatOneDecimalEncoder(json.JSONEncoder):
    """JSON encoder that formats floats to one decimal place."""
    def default(self, obj):
        if isinstance(obj, float):
            return round(obj, 1)
        return super().default(obj)
