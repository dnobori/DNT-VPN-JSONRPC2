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
        CSharpSourceCode cs_types, cs_stubs;

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

        public GeneratedCodeForLang GenerateCodes()
        {
            GeneratedCodeForLang ret = new GeneratedCodeForLang();

            generate_types(ret);

            generate_stubs(ret);

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
