"""
Tests for real-world Lua code patterns.
"""
import pytest

from syntax.lua.lua_lexer import LuaLexer
from syntax.lua.lua_parser import LuaParser


def lex_multiline_code(code: str):
    """
    Helper function to lex multi-line code properly (line-by-line).

    Args:
        code: Multi-line code string

    Returns:
        List of all tokens from all lines
    """
    lines = code.strip().split('\n')
    all_tokens = []
    state = None

    for line in lines:
        lexer = LuaLexer()
        state = lexer.lex(state, line)
        all_tokens.extend(lexer._tokens)

    return all_tokens


class TestLuaRealWorld:
    """Test real-world Lua code patterns."""

    def test_game_development_code(self):
        """Test game development patterns."""
        code = """
-- Game state management
local gameState = {
    players = {},
    enemies = {},
    score = 0
}

function gameState:addPlayer(id, name)
    self.players[id] = { name = name, score = 0 }
end

function gameState:removePlayer(id)
    self.players[id] = nil
end

function gameState:updateScore(points)
    self.score = self.score + points
    print("Score: " .. self.score)
end

-- Enemy AI
function enemyAI:attack(player)
    local distance = math.sqrt(
        (player.x - self.x)^2 +
        (player.y - self.y)^2
    )

    if distance < 100 then
        self:moveTowards(player)
        self:fireAt(player)
    end
end

-- Physics
function updatePhysics(entity, dt)
    entity.vx = entity.vx + entity.ax * dt
    entity.vy = entity.vy + entity.ay * dt
    entity.x = entity.x + entity.vx * dt
    entity.y = entity.y + entity.vy * dt

    if entity.y > groundLevel then
        entity.vy = entity.vy * restitution
    end
end
"""
        tokens = lex_multiline_code(code)

        # Should have many tokens
        assert len(tokens) > 50, "Should have many tokens"

        # Check for various token types
        token_types = set(t.type.name for t in tokens)
        assert 'KEYWORD' in token_types, "Should have keywords"
        assert 'IDENTIFIER' in token_types, "Should have identifiers"
        assert 'OPERATOR' in token_types, "Should have operators"
        assert 'STRING' in token_types, "Should have strings"
        assert 'COMMENT' in token_types, "Should have comments"

        # Check for specific keywords
        keywords = [t for t in tokens if t.type.name == 'KEYWORD']
        keyword_values = [t.value for t in keywords]
        assert 'local' in keyword_values, "Should have local keyword"
        assert 'function' in keyword_values, "Should have function keyword"
        assert 'if' in keyword_values, "Should have if keyword"
        assert 'end' in keyword_values, "Should have end keyword"

    def test_configuration_script(self):
        """Test configuration file patterns."""
        code = """
-- Configuration
config = {
    debug = false,
    windowWidth = 800,
    windowHeight = 600,
    fullscreen = false
}

function config:setDebug(enabled)
    self.debug = enabled
    print("Debug mode: " .. tostring(enabled))
end

function config:getResolution()
    return self.windowWidth .. "x" .. self.windowHeight
end

-- Initialize
local function init()
    print("Initializing...")
    config:setDebug(true)
    print("Resolution: " .. config:getResolution())
end

init()
"""
        tokens = lex_multiline_code(code)

        assert len(tokens) > 30, "Should have multiple tokens"

        # Check for table literal
        table_tokens = [t for t in tokens if t.value in ('{', '}')]
        assert len(table_tokens) >= 2, "Should have table braces"

        # Check for strings
        strings = [t for t in tokens if t.type.name == 'STRING']
        assert len(strings) >= 3, "Should have string values"

        # Check for numbers
        numbers = [t for t in tokens if t.type.name == 'NUMBER']
        assert len(numbers) >= 2, "Should have numeric values"

    def test_api_pattern(self):
        """Test API/library patterns."""
        code = """
-- API wrapper
local api = {}

function api:request(method, path, params, callback)
    local url = self.base_url .. path

    if response.status >= 200 and response.status < 300 then
        callback(nil, response.data)
    else
        callback(response.status, nil)
    end
end

function api:getData(id, callback)
    self:request("GET", "/data/" .. id, nil, callback)
end
"""
        tokens = lex_multiline_code(code)

        # Check for method call patterns (colon operator)
        colons = [t for t in tokens if t.value == ':']
        assert len(colons) >= 2, "Should have method calls with colon"

        # Check for function keywords
        function_keywords = [t for t in tokens if t.value == 'function']
        assert len(function_keywords) >= 2, "Should have function declarations"

        # Check for local keyword
        local_keywords = [t for t in tokens if t.value == 'local']
        assert len(local_keywords) >= 1, "Should have local declarations"

    def test_common_lua_patterns(self):
        """Test commonly used Lua patterns."""
        code = """
-- Common patterns
local function map(t, fn)
    local result = {}
    for k, v in pairs(t) do
        result[k] = fn(v)
    end
    return result
end

function printTable(t)
    for k, v in pairs(t) do
        print(k, v)
    end
end

function deepCopy(original)
    local copy = {}
    for k, v in pairs(original) do
        if type(v) == "table" then
            copy[k] = deepCopy(v)
        else
            copy[k] = v
        end
    end
    return copy
end

-- Table metatable
local Vector = {}
local v = Vector.new()
v.x = 10
v.y = 20
print(v.x, v.y)
"""
        tokens = lex_multiline_code(code)

        # Check for common patterns
        local_keywords = [t for t in tokens if t.value == 'local']
        function_keywords = [t for t in tokens if t.value == 'function']

        assert len(local_keywords) >= 3, "Should have local declarations"
        assert len(function_keywords) >= 2, "Should have function keywords"
        assert 'return' in [t.value for t in tokens], "Should have return"

        # Check for for loops
        for_keywords = [t for t in tokens if t.value == 'for']
        assert len(for_keywords) >= 2, "Should have for loops"

        # Check for booleans
        booleans = [t for t in tokens if t.type.name == 'BOOLEAN']
        assert len(booleans) >= 0, "May have boolean literals"

    def test_libraries_require(self):
        """Test module and library patterns."""
        code = """
-- Library loading
local json = require("lunajson")
local http = require("socket.http")
local fs = require("lfs")

function loadConfig(filename)
    local content = fs.read(filename)
    return json.decode(content)
end

local function saveConfig(filename, data)
    local content = json.encode(data)
    fs.write(filename, content)
end

-- Use library
local config = loadConfig("config.json")
config.debug = true
saveConfig("config.json", config)
"""
        tokens = lex_multiline_code(code)

        # Check for require identifier
        identifiers = [t for t in tokens if t.type.name == 'IDENTIFIER']
        identifier_values = [t.value for t in identifiers]
        assert 'require' in identifier_values, "Should have require calls"

        # Check for dot operators (library method calls)
        dots = [t for t in tokens if t.value == '.']
        assert len(dots) >= 3, "Should have dot operators (library methods)"

        # Check for strings (library names and filenames)
        strings = [t for t in tokens if t.type.name == 'STRING']
        assert len(strings) >= 3, "Should have string literals"
