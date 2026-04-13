#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "openapi-spec-validator>=0.7.1",
# ]
# ///

"""
OpenAPI Schema Validator

This script validates OpenAPI 3.1 specifications using openapi-spec-validator.
Usage: ./validate_openapi.py <openapi_json_file>
"""

import json
import sys

from openapi_spec_validator import validate_spec
from openapi_spec_validator.validation.exceptions import OpenAPIValidationError


def validate_openapi_file(filepath):
    """Validate an OpenAPI specification file."""
    try:
        with open(filepath, 'r') as f:
            spec = json.load(f)

        validate_spec(spec, spec_url='openapi_3.1.0')
        print(f"✅ {filepath} is a valid OpenAPI 3.1 specification")
        return True

    except json.JSONDecodeError as e:
        print(f"❌ Invalid JSON in {filepath}:")
        print(f"   {e}")
        return False
    except OpenAPIValidationError as e:
        print(f"❌ OpenAPI validation failed for {filepath}:")
        print(f"   {e}")
        if hasattr(e, 'path'):
            print(f"   Path: {e.path}")
        if hasattr(e, 'schema_path'):
            print(f"   Schema path: {e.schema_path}")
        return False
    except Exception as e:
        print(f"❌ Unexpected error validating {filepath}:")
        print(f"   {type(e).__name__}: {e}")
        return False


def main():
    if len(sys.argv) != 2:
        print("Usage: ./validate_openapi.py <openapi_json_file>")
        sys.exit(1)

    filepath = sys.argv[1]
    success = validate_openapi_file(filepath)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
