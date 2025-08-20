// Concrete implementation of the hybrid conversation tree approach

class Message {
  constructor(id, content, author, parentMessageId = null) {
    this.id = id;
    this.contentHash = this.generateHash(content);
    this.content = content;
    this.timestamp = new Date();
    this.author = author; // 'user' | 'ai'
    this.parentMessageId = parentMessageId;
  }
  
  generateHash(content) {
    // Simple hash function for demo - use crypto hash in production
    return content.split('').reduce((hash, char) => {
      return ((hash << 5) - hash + char.charCodeAt(0)) & 0xffffffff;
    }, 0).toString(16);
  }
}

class ConversationThread {
  constructor(id, rootMessageId = null) {
    this.id = id;
    this.messageSequence = [];
    this.headMessageId = null;
    this.rootMessageId = rootMessageId;
  }
  
  addMessage(messageId, forkInfo = null) {
    const entry = {
      messageId,
      localIndex: this.messageSequence.length,
      forkInfo
    };
    
    this.messageSequence.push(entry);
    this.headMessageId = messageId;
    
    if (this.rootMessageId === null) {
      this.rootMessageId = messageId;
    }
  }
}

class MessageGraph {
  constructor() {
    this.messages = new Map();
    this.conversations = new Map();
    this.messageToConversations = new Map();
    this.childrenIndex = new Map();
    this.siblingIndex = new Map();
  }
  
  addMessage(message, conversationId, forkInfo = null) {
    // Store the message
    this.messages.set(message.id, message);
    
    // Update parent-child relationships
    if (message.parentMessageId) {
      if (!this.childrenIndex.has(message.parentMessageId)) {
        this.childrenIndex.set(message.parentMessageId, new Set());
      }
      this.childrenIndex.get(message.parentMessageId).add(message.id);
      
      // Update sibling relationships
      this.updateSiblingIndex(message.parentMessageId);
    }
    
    // Add to conversation
    if (!this.conversations.has(conversationId)) {
      this.conversations.set(conversationId, new ConversationThread(conversationId));
    }
    
    this.conversations.get(conversationId).addMessage(message.id, forkInfo);
    
    // Update reverse index
    if (!this.messageToConversations.has(message.id)) {
      this.messageToConversations.set(message.id, new Set());
    }
    this.messageToConversations.get(message.id).add(conversationId);
    
    return message;
  }
  
  updateSiblingIndex(parentMessageId) {
    const children = Array.from(this.childrenIndex.get(parentMessageId) || []);
    children.forEach(childId => {
      this.siblingIndex.set(childId, children.filter(id => id !== childId));
    });
  }
  
  forkConversation(sourceConversationId, forkFromMessageId, newConversationId) {
    const sourceConv = this.conversations.get(sourceConversationId);
    if (!sourceConv) throw new Error('Source conversation not found');
    
    // Find the fork point in the source conversation
    const forkIndex = sourceConv.messageSequence.findIndex(
      entry => entry.messageId === forkFromMessageId
    );
    
    if (forkIndex === -1) throw new Error('Fork message not found in source conversation');
    
    // Create new conversation with shared history up to fork point
    const newConv = new ConversationThread(newConversationId);
    
    // Copy shared history
    for (let i = 0; i <= forkIndex; i++) {
      const entry = sourceConv.messageSequence[i];
      newConv.addMessage(entry.messageId, entry.forkInfo);
      
      // Update reverse index
      if (!this.messageToConversations.has(entry.messageId)) {
        this.messageToConversations.set(entry.messageId, new Set());
      }
      this.messageToConversations.get(entry.messageId).add(newConversationId);
    }
    
    this.conversations.set(newConversationId, newConv);
    return newConv;
  }
  
  createDelegatedConversation(parentConversationId, delegateFromMessageId, delegatedConversationId) {
    const forkInfo = {
      type: 'delegation',
      sourceMessageId: delegateFromMessageId,
      sourceConversation: parentConversationId
    };
    
    // Create new conversation thread
    const delegatedConv = new ConversationThread(delegatedConversationId, delegateFromMessageId);
    this.conversations.set(delegatedConversationId, delegatedConv);
    
    return delegatedConv;
  }
  
  // Query methods
  getMessageHistory(messageId) {
    const history = [];
    let currentId = messageId;
    
    while (currentId) {
      const message = this.messages.get(currentId);
      if (message) {
        history.unshift(message);
        currentId = message.parentMessageId;
      } else {
        break;
      }
    }
    
    return history;
  }
  
  getConversationMessages(conversationId) {
    const conversation = this.conversations.get(conversationId);
    if (!conversation) return [];
    
    return conversation.messageSequence.map(entry => 
      this.messages.get(entry.messageId)
    ).filter(Boolean);
  }
  
  getMessageSiblings(messageId) {
    const siblingIds = this.siblingIndex.get(messageId) || [];
    return siblingIds.map(id => this.messages.get(id)).filter(Boolean);
  }
  
  getSuccessorSiblings(messageId) {
    const children = this.childrenIndex.get(messageId) || new Set();
    return Array.from(children).map(id => this.messages.get(id)).filter(Boolean);
  }
  
  findConversationsContaining(messageId) {
    const conversationIds = this.messageToConversations.get(messageId) || new Set();
    return Array.from(conversationIds).map(id => this.conversations.get(id)).filter(Boolean);
  }
  
  // Debug helper
  printStructure() {
    console.log('=== Message Graph Structure ===');
    console.log('Messages:', this.messages.size);
    console.log('Conversations:', this.conversations.size);
    
    this.conversations.forEach((conv, id) => {
      console.log(`\nConversation ${id}:`);
      conv.messageSequence.forEach(entry => {
        const message = this.messages.get(entry.messageId);
        const indent = '  '.repeat(entry.localIndex);
        console.log(`${indent}- ${message.id}: "${message.content}" (${message.author})`);
        if (entry.forkInfo) {
          console.log(`${indent}  [Fork: ${entry.forkInfo.type} from ${entry.forkInfo.sourceMessageId}]`);
        }
      });
    });
  }
}

// Example usage demonstrating the scenario from the problem
function demonstrateScenario() {
  const graph = new MessageGraph();
  
  // Create initial conversation
  const msgA = new Message('msg-a', 'Hello, I need help with a problem', 'user');
  const msgB = new Message('msg-b', 'I\'d be happy to help! What\'s the problem?', 'ai', 'msg-a');
  
  graph.addMessage(msgA, 'conv-main');
  graph.addMessage(msgB, 'conv-main');
  
  // Fork 1: User asks about technical approach
  graph.forkConversation('conv-main', 'msg-b', 'conv-fork1');
  const msgC1 = new Message('msg-c1', 'It\'s a technical architecture question', 'user', 'msg-b');
  const msgD1 = new Message('msg-d1', 'Let me help you design the architecture', 'ai', 'msg-c1');
  
  graph.addMessage(msgC1, 'conv-fork1');
  graph.addMessage(msgD1, 'conv-fork1');
  
  // Fork 2: User asks about project management
  graph.forkConversation('conv-main', 'msg-b', 'conv-fork2');
  const msgC2 = new Message('msg-c2', 'It\'s a project management question', 'user', 'msg-b');
  const msgD2 = new Message('msg-d2', 'I can help with project planning', 'ai', 'msg-c2');
  
  graph.addMessage(msgC2, 'conv-fork2');
  graph.addMessage(msgD2, 'conv-fork2');
  
  // Delegated conversation from Message B
  graph.createDelegatedConversation('conv-main', 'msg-b', 'conv-delegated');
  const msgE = new Message('msg-e', 'Let me research this topic in depth', 'ai', 'msg-b');
  const msgF = new Message('msg-f', 'Here are my detailed findings...', 'ai', 'msg-e');
  
  graph.addMessage(msgE, 'conv-delegated', {
    type: 'delegation',
    sourceMessageId: 'msg-b',
    sourceConversation: 'conv-main'
  });
  graph.addMessage(msgF, 'conv-delegated');
  
  // Alternative AI response to create siblings
  const msgC3 = new Message('msg-c3', 'Actually, let me clarify the problem first', 'user', 'msg-b');
  graph.addMessage(msgC3, 'conv-main');
  
  // Print the structure
  graph.printStructure();
  
  // Demonstrate queries
  console.log('\n=== Query Examples ===');
  
  console.log('\n1. History before msg-d1:');
  graph.getMessageHistory('msg-d1').forEach(msg => 
    console.log(`  ${msg.id}: "${msg.content}" (${msg.author})`)
  );
  
  console.log('\n2. Siblings of msg-c1:');
  graph.getMessageSiblings('msg-c1').forEach(msg => 
    console.log(`  ${msg.id}: "${msg.content}" (${msg.author})`)
  );
  
  console.log('\n3. Successor siblings of msg-b:');
  graph.getSuccessorSiblings('msg-b').forEach(msg => 
    console.log(`  ${msg.id}: "${msg.content}" (${msg.author})`)
  );
  
  console.log('\n4. Conversations containing msg-b:');
  graph.findConversationsContaining('msg-b').forEach(conv => 
    console.log(`  ${conv.id} (${conv.messageSequence.length} messages)`)
  );
  
  console.log('\n5. Messages in conv-fork1:');
  graph.getConversationMessages('conv-fork1').forEach(msg => 
    console.log(`  ${msg.id}: "${msg.content}" (${msg.author})`)
  );
  
  return graph;
}

// Run the demonstration
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { Message, ConversationThread, MessageGraph, demonstrateScenario };
} else {
  // Run demo if in browser or direct execution
  demonstrateScenario();
}