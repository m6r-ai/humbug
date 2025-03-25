# Copyright 2024 M6R Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from dataclasses import dataclass
from enum import IntEnum

class TokenType(IntEnum):
    """
    Enum-like class representing different types of tokens in the source file.
    """
    NONE: int = 0
    INDENT: int = 1
    OUTDENT: int = 2
    INCLUDE: int = 3
    EMBED: int = 4
    KEYWORD_TEXT: int = 5
    TEXT: int = 6
    ACTION: int = 7
    CONTEXT: int = 8
    ROLE: int = 9
    BAD_INDENT: int = 10
    BAD_OUTDENT: int = 11
    TAB: int = 12
    END_OF_FILE: int = 13


@dataclass(frozen=True)
class Token:
    """
    Represents a token in the input stream.

    Attributes:
        type (TokenType): The type of the token (e.g., TEXT, ACTION).
        value (str): The actual string value of the token.
        input (str): The entire line of input where the token appears.
        filename (str): The file where the token was read from.
        line (int): The line number in the file where the token is located.
        column (int): The column number where the token starts.
    """
    type: TokenType
    value: str
    input: str
    filename: str
    line: int
    column: int
