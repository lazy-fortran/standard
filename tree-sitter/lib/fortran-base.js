/**
 * Tree-sitter Grammar Inheritance System for FORTRAN/Fortran
 * 
 * This provides MUCH better composition than ANTLR4's import mechanism:
 * - True rule extension and override
 * - Additive features without duplication  
 * - Preserves historical accuracy
 * - Allows relaxation of rules (crucial for LazyFortran2025)
 */

/**
 * Extend a grammar with new rules and modifications
 * Better than ANTLR4 because we can:
 * 1. Override rules completely
 * 2. Extend rules with new alternatives
 * 3. Make required rules optional
 * 4. Add new rules that compose with existing ones
 */
function extendGrammar(baseGrammar, extensions) {
  return grammar({
    name: extensions.name || baseGrammar.name,
    
    // Merge extras (whitespace, comments)
    extras: $ => [
      ...(baseGrammar.extras ? baseGrammar.extras($) : []),
      ...(extensions.extras ? extensions.extras($) : [])
    ],

    // Merge external scanners if present
    externals: $ => [
      ...(baseGrammar.externals ? baseGrammar.externals($) : []),
      ...(extensions.externals ? extensions.externals($) : [])
    ],

    // Merge conflicts
    conflicts: $ => [
      ...(baseGrammar.conflicts ? baseGrammar.conflicts($) : []),
      ...(extensions.conflicts ? extensions.conflicts($) : [])
    ],

    // Merge precedences
    precedences: $ => [
      ...(baseGrammar.precedences ? baseGrammar.precedences($) : []),
      ...(extensions.precedences ? extensions.precedences($) : [])
    ],

    // Merge and override rules
    rules: mergeRules(baseGrammar.rules, extensions.rules)
  });
}

/**
 * Merge rules with proper override semantics
 * This is where we're better than ANTLR4!
 */
function mergeRules(baseRules, extensionRules) {
  const merged = {};
  
  // Start with base rules
  for (const [name, rule] of Object.entries(baseRules)) {
    merged[name] = rule;
  }
  
  // Apply extensions
  for (const [name, extension] of Object.entries(extensionRules)) {
    if (typeof extension === 'function') {
      // Direct override
      merged[name] = extension;
    } else if (extension._extend) {
      // Extend existing rule with new alternatives
      const baseRule = merged[name];
      merged[name] = $ => choice(
        baseRule($),
        extension._extend($)
      );
    } else if (extension._prepend) {
      // Add alternatives before existing ones (higher priority)
      const baseRule = merged[name];
      merged[name] = $ => choice(
        extension._prepend($),
        baseRule($)
      );
    } else if (extension._makeOptional) {
      // Make a required rule optional (crucial for LazyFortran2025!)
      const baseRule = merged[name];
      merged[name] = $ => optional(baseRule($));
    } else if (extension._replace) {
      // Complete replacement with access to base
      const baseRule = merged[name];
      merged[name] = $ => extension._replace($, baseRule);
    } else {
      // Default: override
      merged[name] = extension;
    }
  }
  
  return merged;
}

/**
 * Helper to extend a rule with new alternatives
 */
function extend(newRule) {
  return { _extend: newRule };
}

/**
 * Helper to prepend alternatives (higher priority)
 */
function prepend(newRule) {
  return { _prepend: newRule };
}

/**
 * Helper to make a rule optional
 */
function makeOptional() {
  return { _makeOptional: true };
}

/**
 * Helper to replace with access to original
 */
function replace(replacer) {
  return { _replace: replacer };
}

/**
 * Load a base grammar module
 */
function loadGrammar(path) {
  const grammar = require(path);
  // Extract the grammar configuration
  if (typeof grammar === 'function') {
    // It's a grammar() call result
    return grammar;
  } else if (grammar.default) {
    return grammar.default;
  } else if (grammar.grammar) {
    return grammar.grammar;
  } else {
    return grammar;
  }
}

module.exports = {
  extendGrammar,
  extend,
  prepend,
  makeOptional,
  replace,
  loadGrammar
};