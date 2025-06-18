const std = @import("std");

const Lexer = @import("tokenize/lexer.zig").Lexer;

const Parser = @import("parse/parser.zig").Parser;
const AstExprs = @import("parse/ast.zig");

const Transpiler = @import("compile/transpiler.zig").Transpiler;
const Analyzer = @import("analyze/analyzer.zig").Analyzer;

pub fn main() !void {
    var allocator = std.heap.page_allocator;

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.next();

    const filename = args.next() orelse {
        return error.MissingSourceArgument;
    };

    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const buffer = try allocator.alloc(u8, file_size);
    defer allocator.free(buffer);

    _ = try file.readAll(buffer);

    var lexer = Lexer.new(buffer, allocator);
    try lexer.tokenize();
    defer lexer.tokens.deinit();
    lexer.print();

    var parser = try Parser.new(lexer.tokens, allocator);
    _ = try parser.constructAst();
    parser.print();

    var analyzer = Analyzer.new(parser.exprs);
    _ = analyzer.analyze();

    var transpiler = Transpiler.new(parser.exprs);
    _ = try transpiler.compileAndRun(allocator);
}
