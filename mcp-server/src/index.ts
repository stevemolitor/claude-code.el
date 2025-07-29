#!/usr/bin/env node

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { 
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool
} from "@modelcontextprotocol/sdk/types.js";
import * as net from 'net';

/**
 * Connection to Emacs TCP server for tool discovery and execution
 */
class EmacsConnection {
  private socket: net.Socket | null = null;
  private port: number;
  private host: string;
  private requestCounter = 0;
  private pendingRequests = new Map<number, { resolve: Function, reject: Function }>();

  constructor(port: number = 8765, host: string = '127.0.0.1') {
    this.port = port;
    this.host = host;
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      this.socket = new net.Socket();
      
      this.socket.on('connect', () => {
        console.error('Connected to Emacs TCP server');
        resolve();
      });

      this.socket.on('error', (err) => {
        console.error('Emacs connection error:', err.message);
        reject(err);
      });

      this.socket.on('data', (data) => {
        this.handleResponse(data.toString());
      });

      this.socket.on('close', () => {
        console.error('Emacs connection closed');
        this.socket = null;
      });

      this.socket.connect(this.port, this.host);
    });
  }

  private handleResponse(data: string): void {
    const lines = data.trim().split('\n');
    
    for (const line of lines) {
      try {
        const response = JSON.parse(line);
        const requestId = response.id;
        
        if (this.pendingRequests.has(requestId)) {
          const { resolve, reject } = this.pendingRequests.get(requestId)!;
          this.pendingRequests.delete(requestId);
          
          if (response.error) {
            reject(new Error(response.error.message || response.error));
          } else {
            resolve(response.result);
          }
        }
      } catch (err) {
        console.error('Failed to parse Emacs response:', err);
      }
    }
  }

  private async sendRequest(method: string, params: any = {}): Promise<any> {
    if (!this.socket) {
      throw new Error('Not connected to Emacs');
    }

    const id = ++this.requestCounter;
    const request = {
      jsonrpc: '2.0',
      id,
      method,
      params
    };

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });
      
      const requestStr = JSON.stringify(request) + '\n';
      this.socket!.write(requestStr);
      
      // Timeout after 10 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error('Request timeout'));
        }
      }, 10000);
    });
  }

  async discoverTools(): Promise<Tool[]> {
    const tools = await this.sendRequest('discover_tools');
    return tools.map((tool: any) => ({
      name: tool.name,
      description: tool.description || `Execute ${tool.name}`,
      inputSchema: tool.inputSchema || {
        type: 'object',
        properties: {},
        additionalProperties: true
      }
    }));
  }

  async callTool(name: string, arguments_: any): Promise<any> {
    return await this.sendRequest('call_tool', { 
      name, 
      arguments: arguments_ 
    });
  }

  disconnect(): void {
    if (this.socket) {
      this.socket.destroy();
      this.socket = null;
    }
  }
}

/**
 * Main MCP server implementation
 */
class ClaudeCodeMCPServer {
  private server: Server;
  private emacsConnection: EmacsConnection;

  constructor() {
    this.server = new Server(
      {
        name: "claude-code-emacs",
        version: "1.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.emacsConnection = new EmacsConnection();
    this.setupHandlers();
  }

  private setupHandlers(): void {
    // Handle tool listing
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      try {
        const tools = await this.emacsConnection.discoverTools();
        return { tools };
      } catch (error) {
        console.error('Failed to discover tools:', error);
        return { tools: [] };
      }
    });

    // Handle tool execution
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;
      
      try {
        const result = await this.emacsConnection.callTool(name, args || {});
        
        return {
          content: [
            {
              type: "text",
              text: typeof result === 'string' ? result : JSON.stringify(result, null, 2)
            }
          ]
        };
      } catch (error) {
        const errorMessage = error instanceof Error ? error.message : String(error);
        return {
          content: [
            {
              type: "text", 
              text: `Error executing tool ${name}: ${errorMessage}`
            }
          ],
          isError: true
        };
      }
    });
  }

  async start(): Promise<void> {
    // Connect to Emacs first
    try {
      await this.emacsConnection.connect();
    } catch (error) {
      console.error('Failed to connect to Emacs TCP server. Make sure claude-code is running with MCP support enabled.');
      process.exit(1);
    }

    // Start MCP server with stdio transport
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    
    console.error('Claude Code MCP Server started');
  }

  async stop(): Promise<void> {
    this.emacsConnection.disconnect();
    await this.server.close();
  }
}

// Handle graceful shutdown
const server = new ClaudeCodeMCPServer();

process.on('SIGINT', async () => {
  console.error('Shutting down...');
  await server.stop();
  process.exit(0);
});

process.on('SIGTERM', async () => {
  console.error('Shutting down...');
  await server.stop();
  process.exit(0);
});

// Start the server
server.start().catch((error) => {
  console.error('Failed to start server:', error);
  process.exit(1);
});