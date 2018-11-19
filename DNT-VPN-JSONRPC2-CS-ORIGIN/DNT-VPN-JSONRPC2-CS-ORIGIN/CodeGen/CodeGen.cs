﻿using Microsoft.CodeAnalysis;
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

    class GeneratedCode
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

    class GeneratedCodeList
    {
        public GeneratedCode TS = new GeneratedCode();
    }

    class CodeGen
    {
        CSharpSourceCode cs_types;
        CSharpCompiler csc;

        public CodeGen()
        {
            csc = new CSharpCompiler("Test");

            csc.AddReferenceDotNetStandard();
            csc.AddReferenceByType(typeof(Newtonsoft.Json.JsonPropertyAttribute));

            cs_types = new CSharpSourceCode(Path.Combine(CodeGenUtil.ProjectDir, @"VpnServerRpc\VPNServerRpcTypes.cs"));
            csc.AddSourceCode(cs_types);

            csc.Compile();
        }

        void generate_types(GeneratedCodeList ret)
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

                                        default:
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
                                                    ts_type = "number";
                                                    break;

                                                case "String":
                                                    ts_type = "string";
                                                    break;

                                                case "Boolean":
                                                    ts_type = "boolean";
                                                    break;

                                                case "Byte":
                                                    break;

                                                default:
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
                            break;

                        case IMethodSymbol method when method.MethodKind == MethodKind.Constructor:
                            break;

                        default:
                            throw new ApplicationException($"{c.Identifier}.{member.Name}: type = {member.GetType()}");
                    }
                }

                ts.WriteLine("}");
                ts.WriteLine();

                ret.TS.AddPart(c.SpanStart, ts.ToString());
            }
        }

        public GeneratedCodeList GenerateCodes()
        {
            GeneratedCodeList ret = new GeneratedCodeList();

            generate_types(ret);

            return ret;
        }

        public void Test()
        {
            GeneratedCodeList ret = GenerateCodes();

            Console.WriteLine(ret.TS.ToString());

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
