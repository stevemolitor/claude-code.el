#!/usr/bin/env python3
"""
Simple WebSocket client for testing the Emacs WebSocket server.
Requires: pip install websocket-client
"""

import websocket
import json
import time
import sys
import threading

def on_message(ws, message):
    print(f"Received: {message}")
    try:
        data = json.loads(message)
        print(f"Parsed: {json.dumps(data, indent=2)}")
    except:
        pass

def on_error(ws, error):
    print(f"Error: {error}")

def on_close(ws, close_status_code, close_msg):
    print(f"Closed: {close_status_code} - {close_msg}")

def on_open(ws):
    print("Connected!")
    
    def run():
        # Send test messages
        messages = [
            {"type": "test", "message": "Hello from Python client"},
            {"method": "initialize", "id": 1, "params": {"clientInfo": {"name": "test-client"}}},
            {"method": "tools/list", "id": 2, "params": {}},
        ]
        
        for msg in messages:
            print(f"Sending: {json.dumps(msg)}")
            ws.send(json.dumps(msg))
            time.sleep(1)
        
        # Keep connection open for manual testing
        print("\nConnection open. Type messages to send (or 'quit' to exit):")
        
    thread = threading.Thread(target=run)
    thread.start()

def main():
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 12345
    url = f"ws://127.0.0.1:{port}"
    
    print(f"Connecting to {url}...")
    
    ws = websocket.WebSocketApp(url,
                                on_open=on_open,
                                on_message=on_message,
                                on_error=on_error,
                                on_close=on_close,
                                header={"Sec-WebSocket-Protocol": "mcp"})
    
    # Start in separate thread to allow interactive input
    wst = threading.Thread(target=ws.run_forever)
    wst.daemon = True
    wst.start()
    
    # Interactive mode
    try:
        while True:
            user_input = input()
            if user_input.lower() == 'quit':
                break
            elif user_input:
                # Try to parse as JSON, otherwise send as text
                try:
                    msg = json.loads(user_input)
                    ws.send(json.dumps(msg))
                except:
                    ws.send(json.dumps({"type": "chat", "message": user_input}))
    except KeyboardInterrupt:
        pass
    
    print("Closing connection...")
    ws.close()

if __name__ == "__main__":
    main()