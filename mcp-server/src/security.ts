/**
 * Security utilities for claude-code MCP server
 * Provides input sanitization and validation to prevent injection attacks
 */

export class SecurityError extends Error {
    constructor(message: string, public readonly input?: string) {
        super(message);
        this.name = 'SecurityError';
    }
}

/**
 * Dangerous patterns that indicate potential injection attempts
 */
const DANGEROUS_PATTERNS = [
    /"\s*\)\s*;\s*\(/,           // "); (
    /'\s*\)\s*;\s*\(/,           // '); (
    /\\"\s*\)\s*;\s*\(/,         // \"); (
    /\\'\s*\)\s*;\s*\(/,         // \'); (
    /(shell-command|call-process|eval|load-file)/i,
    /(delete-file|delete-directory|write-file)/i,
    /(setq|defun|defvar|defcustom|lambda)/i,
    /^\s*[\(\)]/,                // Starts with parens (potential elisp)
    /\$\{|\$\(/,                 // Shell variable expansion
    /[;&|`]/,                    // Shell command separators
];

/**
 * Validate that a string doesn't contain injection patterns
 */
export function validateNoInjection(input: string, context: string = 'input'): void {
    if (typeof input !== 'string') {
        return; // Only validate strings
    }

    if (input === '') {
        return; // Empty strings are safe
    }

    for (const pattern of DANGEROUS_PATTERNS) {
        if (pattern.test(input)) {
            throw new SecurityError(
                `Potentially dangerous pattern detected in ${context}`, 
                input
            );
        }
    }
}

/**
 * Validate Emacs symbol names
 */
export function validateEmacsSymbol(symbol: string): void {
    if (!symbol || typeof symbol !== 'string') {
        throw new SecurityError('Symbol must be a non-empty string', symbol);
    }

    if (symbol.length > 100) {
        throw new SecurityError('Symbol name too long (max 100 characters)', symbol);
    }

    // Allow only safe characters for Emacs symbols
    if (!/^[a-zA-Z][a-zA-Z0-9\-_:+*/?<>=!]*$/.test(symbol)) {
        throw new SecurityError('Invalid symbol name format', symbol);
    }
}

/**
 * Validate buffer names
 */
export function validateBufferName(bufferName: string): void {
    if (!bufferName || typeof bufferName !== 'string') {
        throw new SecurityError('Buffer name must be a non-empty string', bufferName);
    }

    if (bufferName.length > 256) {
        throw new SecurityError('Buffer name too long (max 256 characters)', bufferName);
    }

    validateNoInjection(bufferName, 'buffer name');
}

/**
 * Validate file paths
 */
export function validateFilePath(filePath: string): void {
    if (!filePath || typeof filePath !== 'string') {
        throw new SecurityError('File path must be a non-empty string', filePath);
    }

    if (filePath.length > 500) {
        throw new SecurityError('File path too long (max 500 characters)', filePath);
    }

    // Block directory traversal
    if (filePath.includes('../') || filePath.includes('..\\')) {
        throw new SecurityError('Directory traversal detected in file path', filePath);
    }

    // Block absolute paths outside allowed directories
    if (filePath.startsWith('/') && !filePath.startsWith('/tmp/ClaudeWorkingFolder/')) {
        throw new SecurityError('Absolute file paths not allowed outside /tmp/ClaudeWorkingFolder/', filePath);
    }

    validateNoInjection(filePath, 'file path');
}

/**
 * Validate search patterns
 */
export function validateSearchPattern(pattern: string): void {
    if (!pattern || typeof pattern !== 'string') {
        throw new SecurityError('Search pattern must be a non-empty string', pattern);
    }

    if (pattern.length > 200) {
        throw new SecurityError('Search pattern too long (max 200 characters)', pattern);
    }

    validateNoInjection(pattern, 'search pattern');
}

/**
 * Validate org content (headings, capture content, etc.)
 */
export function validateOrgContent(content: string): void {
    if (!content || typeof content !== 'string') {
        return; // Allow empty content for some functions
    }

    if (content.length > 5000) {
        throw new SecurityError('Org content too long (max 5000 characters)', content);
    }

    validateNoInjection(content, 'org content');
}

/**
 * Check all arguments recursively for dangerous patterns
 */
export function validateAllArgs(obj: any, path: string = 'args'): void {
    if (typeof obj === 'string') {
        validateNoInjection(obj, path);
        return;
    }
    
    if (Array.isArray(obj)) {
        obj.forEach((item, index) => {
            validateAllArgs(item, `${path}[${index}]`);
        });
        return;
    }
    
    if (obj && typeof obj === 'object') {
        Object.entries(obj).forEach(([key, value]) => {
            validateAllArgs(value, `${path}.${key}`);
        });
        return;
    }
    
    // Numbers, booleans, null, undefined are safe
}

/**
 * Apply tool-specific validation based on known parameter patterns
 */
export function validateToolParameters(toolName: string, args: any): void {
    // First do general validation on all arguments
    validateAllArgs(args, `${toolName}.args`);

    // Apply tool-specific validation
    if (args) {
        // File operations
        if (args.file_paths || args['file-paths']) {
            const filePaths = args.file_paths || args['file-paths'];
            if (Array.isArray(filePaths)) {
                filePaths.forEach((path: string) => validateFilePath(path));
            }
        }

        // Buffer operations
        if (args.buffer_names || args['buffer-names']) {
            const bufferNames = args.buffer_names || args['buffer-names'];
            if (Array.isArray(bufferNames)) {
                bufferNames.forEach((name: string) => validateBufferName(name));
            }
        }

        // Symbol operations
        if (args.symbol_names || args['symbol-names']) {
            const symbolNames = args.symbol_names || args['symbol-names'];
            if (Array.isArray(symbolNames)) {
                symbolNames.forEach((name: string) => validateEmacsSymbol(name));
            }
        }

        if (args.variable_names || args['variable-names']) {
            const variableNames = args.variable_names || args['variable-names'];
            if (Array.isArray(variableNames)) {
                variableNames.forEach((name: string) => validateEmacsSymbol(name));
            }
        }

        // Search operations
        if (args.pattern) {
            validateSearchPattern(args.pattern);
        }

        // Org operations
        if (args.content) {
            validateOrgContent(args.content);
        }

        if (args.heading_text || args['heading-text']) {
            const headingText = args.heading_text || args['heading-text'];
            validateOrgContent(headingText);
        }

        // Single file path
        if (args.org_file || args['org-file']) {
            const orgFile = args.org_file || args['org-file'];
            validateFilePath(orgFile);
        }
    }
}