"""Transcript writer implementation for Humbug application."""

import json


class FloatOneDecimalEncoder(json.JSONEncoder):
    """JSON encoder that formats floats to one decimal place."""
    def default(self, o):
        """
        Convert object to JSON serializable form.

        Args:
            o: Object to convert

        Returns:
            JSON serializable representation
        """
        if isinstance(o, float):
            return round(o, 1)

        return super().default(o)
