#!/usr/bin/env node

// Simple test script for security validation
import { validateToolParameters, SecurityError } from './dist/security.js';

console.log('Testing MCP Security Validation...\n');

const maliciousInputs = [
    {
        name: 'Basic injection test',
        toolName: 'mcp-hello-world',
        args: { name: '"); (shell-command "rm -rf /important_folder"' },
        shouldFail: true
    },
    {
        name: 'Elisp eval injection',
        toolName: 'mcp-get-variable-value',
        args: { 'variable-names': ['(eval "(delete-file \\"/important-file\\"))")'] },
        shouldFail: true
    },
    {
        name: 'Shell command injection in file path',
        toolName: 'mcp-open-file',
        args: { 'file-paths': ['/tmp/test; rm -rf /important_folder'] },
        shouldFail: true
    },
    {
        name: 'Elisp function call',
        toolName: 'mcp-hello-world',
        args: { name: '(message "test")' },
        shouldFail: true
    },
    {
        name: 'Legitimate usage',
        toolName: 'mcp-hello-world',
        args: { name: 'Claude' },
        shouldFail: false
    },
    {
        name: 'Legitimate file path',
        toolName: 'mcp-open-file',
        args: { 'file-paths': ['/tmp/ClaudeWorkingFolder/test.txt'] },
        shouldFail: false
    },
    {
        name: 'Legitimate variable access',
        toolName: 'mcp-get-variable-value',
        args: { 'variable-names': ['user-full-name', 'emacs-version'] },
        shouldFail: false
    },
    {
        name: 'Variables with plus signs',
        toolName: 'mcp-get-variable-value',
        args: { 'variable-names': ['c++-mode-hook', 'c++-tab-always-indent'] },
        shouldFail: false
    },
    {
        name: 'Variables with forward slashes',
        toolName: 'mcp-get-variable-value',
        args: { 'variable-names': ['find/grep-command', 'w3m/lynx-command'] },
        shouldFail: false
    },
    {
        name: 'Variables with mixed special chars',
        toolName: 'mcp-get-variable-value',
        args: { 'variable-names': ['org-agenda/clockreport-mode', 'evil-want-C-u-scroll'] },
        shouldFail: false
    },
    {
        name: 'Buffer names with brackets',
        toolName: 'mcp-view-buffer',
        args: { 'buffer-names': ['*Messages*', '*scratch*'] },
        shouldFail: false
    }
];

let passed = 0;
let failed = 0;

maliciousInputs.forEach(test => {
    try {
        validateToolParameters(test.toolName, test.args);
        if (test.shouldFail) {
            console.log(`❌ FAIL: ${test.name} - Expected security error but validation passed`);
            failed++;
        } else {
            console.log(`✅ PASS: ${test.name} - Legitimate input accepted`);
            passed++;
        }
    } catch (error) {
        if (error instanceof SecurityError) {
            if (test.shouldFail) {
                console.log(`✅ PASS: ${test.name} - Security error correctly caught: ${error.message}`);
                passed++;
            } else {
                console.log(`❌ FAIL: ${test.name} - Legitimate input rejected: ${error.message}`);
                failed++;
            }
        } else {
            console.log(`❌ ERROR: ${test.name} - Unexpected error: ${error.message}`);
            failed++;
        }
    }
});

console.log(`\nResults: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
