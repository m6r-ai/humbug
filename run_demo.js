// Load and run the conversation tree demonstration
const fs = require('fs');

// Read and execute the implementation
const implementationCode = fs.readFileSync('conversation_tree_implementation.js', 'utf8');
eval(implementationCode);

// Run the demonstration
console.log('Running Conversation Tree Demonstration...\n');
demonstrateScenario();