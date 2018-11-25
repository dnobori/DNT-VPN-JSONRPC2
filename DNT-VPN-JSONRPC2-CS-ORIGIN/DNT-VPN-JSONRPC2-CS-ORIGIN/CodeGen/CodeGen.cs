using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Linq;
using static System.Console;

namespace DNT_VPN_JSONRPC2_CS_ORIGIN.CodeGen
{
    static class CodeGenUtil
    {
        public static string AppExeDir;
        public static string ProjectDir;

        static CodeGenUtil()
        {
            AppExeDir = System.AppContext.BaseDirectory;
            ProjectDir = AppExeDir;
            string tmp = AppExeDir;
            while (true)
            {
                try
                {
                    tmp = Path.GetDirectoryName(tmp);
                    if (Directory.GetFiles(tmp, "*.csproj").Length >= 1)
                    {
                        ProjectDir = tmp;
                        break;
                    }
                }
                catch
                {
                    break;
                }
            }
        }
    }

    class CSharpSourceCode
    {
        public SyntaxTree Tree { get; }
        public CompilationUnitSyntax Root { get; }
        public SemanticModel Model { get; set; }

        public CSharpSourceCode(string filename) : this(File.ReadAllText(filename), filename)
        {
        }

        public CSharpSourceCode(string body, string filename)
        {
            this.Tree = CSharpSyntaxTree.ParseText(body, path: filename);
            this.Root = this.Tree.GetCompilationUnitRoot();
        }
    }


    class CSharpCompiler
    {
        public string AssemblyName { get; }
        public List<MetadataReference> ReferencesList { get; } = new List<MetadataReference>();
        public List<CSharpSourceCode> SourceCodeList { get; } = new List<CSharpSourceCode>();

        CSharpCompilation _compilation = null;

        public CSharpCompilation Compilation
        {
            get
            {
                if (_compilation == null)
                {
                    _compilation = CSharpCompilation.Create(this.AssemblyName,
                        this.SourceCodeList.Select(s => s.Tree),
                        this.ReferencesList,
                        options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, optimizationLevel: OptimizationLevel.Debug,
                        assemblyIdentityComparer: DesktopAssemblyIdentityComparer.Default));

                }
                return _compilation;
            }
        }

        public CSharpCompiler(string assembly_name)
        {
            this.AssemblyName = assembly_name;
        }

        public void AddReference(MetadataReference r)
        {
            this.ReferencesList.Add(r);
        }
        public void AddReferenceByPath(string path)
        {
            AddReference(MetadataReference.CreateFromFile(path));
        }
        public void AddReferenceByType(Type type)
        {
            AddReferenceByPath(type.Assembly.Location);
        }
        public void AddReferenceByAssemblyName(string name)
        {
            var a = System.Reflection.Assembly.Load(new System.Reflection.AssemblyName(name));

            AddReferenceByPath(a.Location);
        }
        public void AddReferenceDotNetStandard()
        {
            var a = System.Reflection.Assembly.Load(new System.Reflection.AssemblyName("netstandard"));

            AddReferenceByPath(a.Location);

            string dir = Path.GetDirectoryName(a.Location);

            AddReferenceByPath(Path.Combine(dir, "System.Private.CoreLib.dll"));
            
            foreach (var refa in a.GetReferencedAssemblies())
            {
                string dll_name = Path.Combine(dir, refa.Name) + ".dll";

                if (File.Exists(dll_name))
                {
                    AddReferenceByPath(dll_name);
                }
            }
        }

        public void AddSourceCode(CSharpSourceCode cs)
        {
            this.SourceCodeList.Add(cs);
        }

        public bool OkOrPrintErrors()
        {
            MemoryStream ms = new MemoryStream();
            Microsoft.CodeAnalysis.Emit.EmitResult ret = Compilation.Emit(ms);

            if (ret.Success)
            {
                return true;
            }

            IEnumerable<Diagnostic> failures = ret.Diagnostics.Where(diagnostic =>
                        diagnostic.IsWarningAsError ||
                        diagnostic.Severity == DiagnosticSeverity.Error);

            foreach (Diagnostic diagnostic in failures)
            {
                WriteLine(diagnostic.ToString());
            }
            return false;
        }

        public void Compile(bool test_full_compile = false)
        {
            if (test_full_compile)
            {
                if (OkOrPrintErrors() == false)
                {
                    throw new ApplicationException("Compile Error.");
                }
            }

            foreach (CSharpSourceCode cs in this.SourceCodeList)
            {
                cs.Model = this.Compilation.GetSemanticModel(cs.Tree);
            }
        }
    }

    class GeneratedCodePart
    {
        public int Seq = 0;
        public string Text = "";
    }

    class GeneratedCodeSection
    {
        public List<GeneratedCodePart> PartList = new List<GeneratedCodePart>();

        public override string ToString()
        {
            StringWriter w = new StringWriter();
            var a = this.PartList.OrderBy(x => x.Seq);

            foreach (var b in a)
            {
                w.Write(b.Text.ToString());
            }

            return w.ToString();
        }

        public void AddPart(int seq, string text)
        {
            this.PartList.Add(new GeneratedCodePart() { Seq = seq, Text = text });
        }
    }

    class GeneratedCode
    {
        public GeneratedCodeSection Types = new GeneratedCodeSection();
        public GeneratedCodeSection Stubs = new GeneratedCodeSection();
        public GeneratedCodeSection Tests = new GeneratedCodeSection();

        public override string ToString()
        {
            StringWriter w = new StringWriter();

            w.WriteLine("// --- Types ---");
            w.Write(this.Types.ToString());
            w.WriteLine();

            w.WriteLine("// --- Stubs ---");
            w.Write(this.Stubs.ToString());
            w.WriteLine();

            w.WriteLine("// --- Tests ---");
            w.Write(this.Tests.ToString());
            w.WriteLine();

            return w.ToString();
        }
    }

    class GeneratedCodeForLang
    {
        public GeneratedCode TypeScript = new GeneratedCode();
    }

    class CodeGen
    {
        CSharpSourceCode cs_types, cs_stubs, cs_tests;

        CSharpCompiler csc;

        public CodeGen()
        {
            csc = new CSharpCompiler("Test");

            csc.AddReferenceDotNetStandard();
            csc.AddReferenceByType(typeof(Newtonsoft.Json.JsonPropertyAttribute));

            cs_types = new CSharpSourceCode(Path.Combine(CodeGenUtil.ProjectDir, @"VpnServerRpc\VPNServerRpcTypes.cs"));
            csc.AddSourceCode(cs_types);

            cs_stubs = new CSharpSourceCode(Path.Combine(CodeGenUtil.ProjectDir, @"VpnServerRpc\VPNServerRpc.cs"));
            csc.AddSourceCode(cs_stubs);

            cs_tests = new CSharpSourceCode(Path.Combine(CodeGenUtil.ProjectDir, @"VpnServerRpcTest\VpnServerRpcTest.cs"));
            csc.AddSourceCode(cs_tests);

            csc.Compile();
        }

        void generate_types(GeneratedCodeForLang ret)
        {
            var model = cs_types.Model;

            var class_list = cs_types.Root.DescendantNodes().OfType<ClassDeclarationSyntax>();

            foreach (ClassDeclarationSyntax c in class_list)
            {
                StringWriter ts = new StringWriter();

                ts.WriteLine($"export class {c.Identifier.Text}");
                ts.WriteLine("{");

                foreach (var member in model.GetDeclaredSymbol(c).GetMembers())
                {
                    switch (member)
                    {
                        case IFieldSymbol field:
                            string ts_type = "";
                            ITypeSymbol type = field.Type;
                            switch (type.Kind)
                            {
                                case SymbolKind.NamedType:
                                    switch (type.Name)
                                    {
                                        case "UInt32":
                                        case "UInt64":
                                            ts_type = "number";
                                            break;

                                        case "String":
                                            ts_type = "string";
                                            break;

                                        case "Boolean":
                                            ts_type = "boolean";
                                            break;

                                        case "DateTime":
                                            ts_type = "Date";
                                            break;

                                        default:
                                            if (type.TypeKind == TypeKind.Enum)
                                            {
                                                ts_type = type.Name;
                                                break;
                                            }
                                            throw new ApplicationException($"{c.Identifier}.{member.Name}: type.Name = {type.Name}");
                                    }
                                    break;

                                case SymbolKind.ArrayType:
                                    ITypeSymbol type2 = ((IArrayTypeSymbol)type).ElementType;

                                    switch (type2.Kind)
                                    {
                                        case SymbolKind.NamedType:
                                            switch (type2.Name)
                                            {
                                                case "UInt32":
                                                case "UInt64":
                                                    ts_type = "number[]";
                                                    break;

                                                case "String":
                                                    ts_type = "string[]";
                                                    break;

                                                case "Boolean":
                                                    ts_type = "boolean[]";
                                                    break;

                                                case "Byte":
                                                    ts_type = "Uint8Array";
                                                    break;

                                                default:
                                                    if (type2.ContainingAssembly.Name == csc.AssemblyName)
                                                    {
                                                        ts_type = type2.Name + "[]";
                                                        break;
                                                    }
                                                    throw new ApplicationException($"{c.Identifier}.{member.Name}: type2.Name = {type2.Name}");
                                            }
                                            break;

                                        default:
                                            throw new ApplicationException($"{c.Identifier}.{member.Name}: type2.Kind = {type2.Kind}");
                                    }

                                    break;

                                default:
                                    throw new ApplicationException($"{c.Identifier}.{member.Name}: type.Kind = {type.Kind}");
                            }

                            if (string.IsNullOrEmpty(ts_type) == false)
                            {
                                ts.WriteLine($"    {field.Name}?: {ts_type};");
                            }
                            break;

                        case IMethodSymbol method when method.MethodKind == MethodKind.Constructor:
                            break;

                        default:
                            throw new ApplicationException($"{c.Identifier}.{member.Name}: type = {member.GetType()}");
                    }
                }

                ts.WriteLine("}");
                ts.WriteLine();

                ret.TypeScript.Types.AddPart(c.SpanStart, ts.ToString());
            }

            var enum_list = cs_types.Root.DescendantNodes().OfType<EnumDeclarationSyntax>();

            foreach (EnumDeclarationSyntax e in enum_list)
            {
                StringWriter ts = new StringWriter();

                ts.WriteLine($"export enum {e.Identifier.Text}");
                ts.WriteLine("{");

                foreach (var member in model.GetDeclaredSymbol(e).GetMembers())
                {
                    switch (member)
                    {
                        case IFieldSymbol field:
                            if (field.IsConst && field.IsDefinition)
                            {
                                ts.WriteLine($"    {field.Name} = {field.ConstantValue},");
                            }
                            break;
                    }
                }

                ts.WriteLine("}");
                ts.WriteLine();

                ret.TypeScript.Types.AddPart(e.SpanStart, ts.ToString());
            }
        }

        void generate_stubs(GeneratedCodeForLang ret)
        {
            var model = cs_stubs.Model;

            var rpc_class = cs_stubs.Root.DescendantNodes().OfType<ClassDeclarationSyntax>().Where(c => c.Identifier.Text == "VpnServerRpc").First();

            var members = model.GetDeclaredSymbol(rpc_class).GetMembers();

            var methods = members.Where(m => m is IMethodSymbol).Select(m => m as IMethodSymbol).Where(m => m.IsStatic == false)
                .Where(m => m.IsAsync).Where(m => m.Name != "CallAsync");

            foreach (var method in methods)
            {
                string method_name = method.Name;
                if (method_name.EndsWith("Async") == false) throw new ApplicationException($"{method.Name}: method_name = {method_name}");
                method_name = method_name.Substring(0, method_name.Length - 5);

                INamedTypeSymbol ret_type = (INamedTypeSymbol)method.ReturnType;
                if (ret_type.Name != "Task") throw new ApplicationException($"{method.Name}: ret_type.Name = {ret_type.Name}");

                var ret_type_args = ret_type.TypeArguments;
                if (ret_type_args.Length != 1) throw new ApplicationException($"{method.Name}: type_args.Length = {ret_type_args.Length}");

                var ret_type_name = ret_type_args[0].Name;

                if (method.Parameters.Length >= 2) throw new ApplicationException($"{method.Name}: method.Parameters.Length = {method.Parameters.Length}");

                if (method.DeclaringSyntaxReferences.Length != 1) throw new ApplicationException($"{method.Name}: method.DeclaringSyntaxReferences.Length = {method.DeclaringSyntaxReferences.Length}");

                MethodDeclarationSyntax syntax = (MethodDeclarationSyntax)method.DeclaringSyntaxReferences[0].GetSyntax();
                if (syntax.Body != null) throw new ApplicationException($"{method.Name}: syntax.Body != null");
                if (syntax.ExpressionBody == null) throw new ApplicationException($"{method.Name}: syntax.ExpressionBody == null");

                ArrowExpressionClauseSyntax body = syntax.ExpressionBody;
                InvocationExpressionSyntax invoke = body.DescendantNodes().OfType<InvocationExpressionSyntax>().Single();

                if (model.GetSymbolInfo(invoke.Expression).Symbol.Name != "CallAsync") throw new ApplicationException($"{method.Name}: model.GetSymbolInfo(invoke.Expression).Symbol.Name = {model.GetSymbolInfo(invoke.Expression).Symbol.Name}");

                if (invoke.ArgumentList.Arguments.Count != 2) throw new ApplicationException($"{method.Name}: invoke.ArgumentList.Arguments.Count = {invoke.ArgumentList.Arguments.Count}");

                LiteralExpressionSyntax str_syntax = (LiteralExpressionSyntax)invoke.ArgumentList.Arguments[0].Expression;

                string str = str_syntax.Token.Text;

                StringWriter ts = new StringWriter();

                if (method.Parameters.Length == 0)
                {
                    ts.WriteLine($"    public {method_name} = (): Promise<{ret_type_name}> =>");
                    ts.WriteLine("    {");
                    ts.WriteLine($"        return this.CallAsync<{ret_type_name}>({str}, new {ret_type_name}());");
                    ts.WriteLine("    }");
                    ts.WriteLine("    ");
                }
                else
                {
                    ts.WriteLine($"    public {method_name} = (in_param: {ret_type_name}): Promise<{ret_type_name}> =>");
                    ts.WriteLine("    {");
                    ts.WriteLine($"        return this.CallAsync<{ret_type_name}>({str}, in_param);");
                    ts.WriteLine("    }");
                    ts.WriteLine("    ");
                }

                ret.TypeScript.Stubs.AddPart(method.DeclaringSyntaxReferences[0].Span.Start, ts.ToString());
            }
        }

        class MyWalker : CSharpSyntaxWalker
        {
            List<(string line, int depth)> codes = new List<(string line, int depth)>();
            protected int CurrentDepth = 0;
            const int TabSpace = 4;
            protected CSharpSourceCode Src;

            public MyWalker(CSharpSourceCode src) : base(SyntaxWalkerDepth.StructuredTrivia)
            {
                this.Src = src;
            }

            public void Emit(string line)
            {
                this.codes.Add((line, this.CurrentDepth));
            }

            public override string ToString()
            {
                StringWriter w = new StringWriter();
                this.codes.ForEach(x =>
                {
                    w.WriteLine(new string(' ', TabSpace * x.depth) + x.line);
                });
                return w.ToString();
            }

            public void MergeCodesFrom(MyWalker other)
            {
                other.codes.ForEach(x =>
                {
                    this.codes.Add((x.line, x.depth + 1));
                });
            }
        }

        class ExpressionWalker : MyWalker
        {
            public ExpressionWalker(CSharpSourceCode src) : base(src) { }

            public override void VisitExpressionStatement(ExpressionStatementSyntax node)
            {
                base.VisitExpressionStatement(node);
            }
        }

        class BlockWalker : MyWalker
        {
            public BlockWalker(CSharpSourceCode src) : base(src) { }

            public override void VisitBlock(BlockSyntax node)
            {
                base.VisitBlock(node);
            }

            public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
            {
                var dec = node.Declaration;
                var type = dec.Type;
                var vars = dec.Variables;

                foreach (var v in vars)
                {
                    var type_dec = Src.Model.GetTypeInfo(type);
                    var v_dec = Src.Model.GetDeclaredSymbol(v);
                    Emit($"{type_dec.Type.Name} {v_dec.Name};");
                }
            }
        }

        class MethodWalker : MyWalker
        {
            public MethodWalker(CSharpSourceCode src) : base(src) { }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                var m = Src.Model.GetDeclaredSymbol(node);

                string params_str = "";
                var param_list = m.Parameters;
                foreach (var param in param_list)
                {
                    if (string.IsNullOrEmpty(params_str) == false) params_str += ", ";

                    params_str += $"{param.Type.Name} {param.Name}";
                }

                Emit($"{m.ReturnType.Name} {m.Name}({params_str})");
                Emit("{");

                //base.VisitMethodDeclaration(node);

                BlockWalker w = new BlockWalker(Src);
                w.VisitBlock(node.Body);
                MergeCodesFrom(w);

                Emit("}");
            }
        }

        class ClassWalker : MyWalker
        {
            public ClassWalker(CSharpSourceCode src) : base(src) { }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                MethodWalker w = new MethodWalker(Src);
                w.VisitMethodDeclaration(node);
                MergeCodesFrom(w);
            }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                Emit($"class {node.Identifier.Text}");
                Emit("{");
                base.VisitClassDeclaration(node);
                Emit("}");
            }
        }

        class TestWalker2 : MyWalker
        {
            public TestWalker2(CSharpSourceCode src) : base(src) { }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                ClassWalker w = new ClassWalker(Src);
                w.Visit(node);
                MergeCodesFrom(w);
                //base.VisitClassDeclaration(node);
            }
        }

        class TestWalker :  CSharpSyntaxWalker
        {
            StringWriter w = new StringWriter();

            List<string> lines = new List<string>();
            string current_line = "";
            int current_depth = 0;
            const int TabSpace = 4;
            CSharpSourceCode src;

            public TestWalker(CSharpSourceCode src) : base(SyntaxWalkerDepth.StructuredTrivia)
            {
                this.src = src;
            }

            void _emit_internal(string str, bool new_line)
            {
                if (string.IsNullOrEmpty(current_line))
                {
                    current_line += new string(' ', current_depth * TabSpace);
                }
                current_line += str;
                if (new_line)
                {
                    lines.Add(current_line);
                    current_line = "";
                }
            }

            void emit_line(string str) => emit(str + "\r\n");

            void emit(string str, bool new_line)
            {
                if (new_line == false)
                {
                    emit(str);
                }
                else
                {
                    emit_line(str);
                }
            }

            void emit(string str)
            {
                string tmp = "";
                for (int i = 0; i < str.Length; i++)
                {
                    char c = str[i];
                    if (c == '\r') { }
                    else if (c == '\n')
                    {
                        _emit_internal(tmp, true);
                        tmp = "";
                    }
                    else
                    {
                        tmp += c;
                    }
                }
                if (String.IsNullOrEmpty(tmp) == false)
                {
                    _emit_internal(tmp, false);
                }
            }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                emit("public");
                emit(" ");
                Visit(node.ReturnType);
                emit(" ");
                emit(node.Identifier.Text);
                Visit(node.ParameterList);
                emit_line("");

                Visit(node.Body);
            }

            public override void VisitParameter(ParameterSyntax node)
            {
                Visit(node.Type);
                emit(" ");
                emit($"{node.Identifier.Text}");
            }

            public override void VisitParameterList(ParameterListSyntax node)
            {
                emit("(");
                int num = 0;
                foreach (ParameterSyntax p in node.Parameters)
                {
                    if (num >= 1)
                    {
                        emit(", ");
                    }

                    Visit(p);

                    num++;
                }
                emit(")");
            }

            public override void VisitArgumentList(ArgumentListSyntax node)
            {
                emit("(");
                int num = 0;
                foreach (ArgumentSyntax arg in node.Arguments)
                {
                    if (num >= 1)
                    {
                        emit(", ");
                    }

                    this.VisitArgument(arg);

                    num++;
                }
                emit(")");
            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                Visit(node.Left);

                emit(" = ");

                Visit(node.Right);
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                Visit(node.Expression);

                emit(node.OperatorToken.Text);

                Visit(node.Name);
            }

            public override void VisitCastExpression(CastExpressionSyntax node)
            {
                emit("(");
                Visit(node.Type);
                emit(")");
                Visit(node.Expression);
            }

            public override void VisitBreakStatement(BreakStatementSyntax node)
            {
                emit_line("break;");
            }

            public override void VisitReturnStatement(ReturnStatementSyntax node)
            {
                if (node.Expression == null)
                {
                    emit_line("return;");
                }
                else
                {
                    emit("return");
                    emit(" ");
                    Visit(node.Expression);
                    emit_line(";");
                }
            }

            public override void VisitForEachStatement(ForEachStatementSyntax node)
            {
                emit("foreach (");

                Visit(node.Type);

                emit(" ");

                emit(node.Identifier.Text);

                emit(" in ");

                Visit(node.Expression);

                emit_line(")");

                Visit(node.Statement);
            }

            public override void VisitExpressionStatement(ExpressionStatementSyntax node)
            {
                Visit(node.Expression);

                emit_line(";");
            }

            public override void VisitConditionalExpression(ConditionalExpressionSyntax node)
            {
                Visit(node.Condition);
                emit(" ? ");
                Visit(node.WhenTrue);
                emit(" : ");
                Visit(node.WhenFalse);
            }

            public override void VisitIfStatement(IfStatementSyntax node)
            {
                emit("if (");
                Visit(node.Condition);
                emit_line(")");

                Visit(node.Statement);

                if (node.Else != null)
                {
                    if (node.Else.Statement is IfStatementSyntax)
                    {
                        emit("else ");
                    }
                    else
                    {
                        emit_line("else");
                    }

                    Visit(node.Else.Statement);
                }
            }

            public override void VisitInitializerExpression(InitializerExpressionSyntax node)
            {
                if (node.Kind() == SyntaxKind.ArrayInitializerExpression)
                {
                    emit("{ ");
                    current_depth++;

                    foreach (var exp in node.Expressions)
                    {
                        this.Visit(exp);

                        emit(", ");
                    }

                    current_depth--;
                    emit(" }");
                }
                else
                {
                    emit_line("{");
                    current_depth++;

                    foreach (var exp in node.Expressions)
                    {
                        this.Visit(exp);

                        emit_line(",");
                    }

                    current_depth--;
                    emit("}");
                }
            }

            public override void VisitArrayCreationExpression(ArrayCreationExpressionSyntax node)
            {
                var type = node.Type;

                emit("new ");

                Visit(node.Type);

                if (node.Initializer != null)
                {
                    emit(" ");
                    Visit(node.Initializer);
                }
            }

            public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                var type = (IdentifierNameSyntax)node.Type;

                emit($"new {type.Identifier.Text}");

                Visit(node.ArgumentList);

                if (node.Initializer != null)
                {
                    emit_line("");
                    Visit(node.Initializer);
                }
            }

            public override void VisitLiteralExpression(LiteralExpressionSyntax node)
            {
                emit(node.Token.Text);
            }

            public override void VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
            {
                emit("(");
                base.Visit(node.Expression);
                emit(")");
            }

            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                base.Visit(node.Left);
                emit($" {node.OperatorToken.Text} ");
                base.Visit(node.Right);
            }

            public override void VisitIdentifierName(IdentifierNameSyntax node)
            {
                emit($"{node.Identifier.Text}");
            }

            public override void VisitPredefinedType(PredefinedTypeSyntax node)
            {
                emit($"{node.Keyword.Text}");
            }

            public override void VisitArrayRankSpecifier(ArrayRankSpecifierSyntax node)
            {
                emit("[");

                int num = 0;

                foreach (ExpressionSyntax exp in node.Sizes)
                {
                    if (num >= 1)
                    {
                        emit(",");
                    }

                    Visit(exp);

                    num++;
                }

                emit("]");
            }

            public override void VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
            {
            }

            public override void VisitArrayType(ArrayTypeSyntax node)
            {
                Visit(node.ElementType);

                foreach (var rank in node.RankSpecifiers)
                {
                    Visit(rank);
                }
            }

            public void VisitVariableDeclarator(VariableDeclaratorSyntax node, TypeSyntax type)
            {
                var type_dec = src.Model.GetTypeInfo(type);

                if (type is PredefinedTypeSyntax)
                {
                    Visit(type);
                }
                else if (type is ArrayTypeSyntax)
                {
                    Visit(type);
                }
                else if (type is IdentifierNameSyntax)
                {
                    Visit(type);
                }
                else
                {
                    throw new ApplicationException($"VisitVariableDeclarator: {type.GetType().ToString()}");
                }

                emit($" {node.Identifier.Text}");

                if (node.Initializer != null)
                {
                    emit(" = ");

                    var value = node.Initializer.Value;

                    base.Visit(value);
                }

                emit_line(";");
            }

            public override void VisitVariableDeclaration(VariableDeclarationSyntax node)
            {
                foreach (var v in node.Variables)
                {
                    //emit($"{type_dec.Type.Name} {v.Identifier.Text}");

                    VisitVariableDeclarator(v, node.Type);

                    //emit_line(";");
                }
            }

            public override void VisitBlock(BlockSyntax node)
            {
                emit_line("{");
                current_depth++;

                foreach (var statement in node.Statements)
                {
                    Visit(statement);
                }

                current_depth--;
                emit_line("}");
            }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                emit_line($"class {node.Identifier.Text}");
                emit_line("{");

                current_depth++;

                base.VisitClassDeclaration(node);

                current_depth--;

                emit_line("}");
            }

            public override string ToString()
            {
                StringWriter w = new StringWriter();
                this.lines.ForEach(x => w.WriteLine(x));
                if (String.IsNullOrEmpty(this.current_line) == false) w.WriteLine(this.current_line);
                return w.ToString();
            }
        }

        void generate_tests(GeneratedCodeForLang ret)
        {
            TestWalker w = new TestWalker(cs_tests);

            var test_class = cs_tests.Root.DescendantNodes().OfType<ClassDeclarationSyntax>().Where(c => c.Identifier.Text == "VPNRPCTest").First();

            w.Visit(test_class);

            WriteLine(w.ToString());
        }

        public GeneratedCodeForLang GenerateCodes()
        {
            GeneratedCodeForLang ret = new GeneratedCodeForLang();

            //generate_types(ret);

            //generate_stubs(ret);

            generate_tests(ret);

            return ret;
        }

        public void Test()
        {
            GeneratedCodeForLang ret = GenerateCodes();

            Console.WriteLine(ret.TypeScript.ToString());

            return;
            var model = cs_types.Model;

            var type_classes = cs_types.Root.DescendantNodes()
                .OfType<ClassDeclarationSyntax>();

            foreach (ClassDeclarationSyntax v in type_classes)
            {
                WriteLine(v.Identifier.Text);

                var info = model.GetDeclaredSymbol(v);

                var x = info.GetMembers();

                foreach (var y in x)
                {
                    WriteLine(y.Name);
                }

                break;
            }

            Console.WriteLine();
        }
    }
}
