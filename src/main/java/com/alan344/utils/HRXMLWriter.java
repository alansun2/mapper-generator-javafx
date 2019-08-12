package com.alan344.utils;

import org.dom4j.Comment;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.Node;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.XMLWriter;
import org.dom4j.tree.NamespaceStack;

import java.io.IOException;
import java.io.Writer;

public class HRXMLWriter extends XMLWriter {
    /**
     * The Stack of namespaceStack written so far
     */
    private NamespaceStack namespaceStack = new NamespaceStack();
    /**
     * The initial number of indentations (so you can print a whole document
     * indented, if you like)
     */
    private int indentLevel = 0;


    /** The format used by this writer */
    private OutputFormat format;

    public HRXMLWriter(Writer wr, OutputFormat format) {
        super(wr, format);
        this.format = format;
    }

    @Override
    protected void indent() throws IOException {
        String indent = format.getIndent();

        if ((indent != null) && (indent.length() > 0)) {
            for (int i = 0; i < indentLevel; i++) {
                writer.write(indent);
            }
        }
    }

    @Override
    protected void writeElement(Element element) throws IOException {
        int size = element.nodeCount();
        String qualifiedName = element.getQualifiedName();

        writePrintln();
        indent();

        writer.write("<");
        writer.write(qualifiedName);

        int previouslyDeclaredNamespaces = namespaceStack.size();
        Namespace ns = element.getNamespace();

        if (isNamespaceDeclaration(ns)) {
            namespaceStack.push(ns);
            writeNamespace(ns);
        }

        // Print out additional namespace declarations
        boolean textOnly = true;

        for (int i = 0; i < size; i++) {
            Node node = element.node(i);

            if (node instanceof Namespace) {
                Namespace additional = (Namespace) node;

                if (isNamespaceDeclaration(additional)) {
                    namespaceStack.push(additional);
                    writeNamespace(additional);
                }
            } else if (node instanceof Element) {
                textOnly = false;
            } else if (node instanceof Comment) {
                textOnly = false;
            }
        }

        writeAttributes(element);

        lastOutputNodeType = Node.ELEMENT_NODE;

        if (size <= 0) {
            writeEmptyElementClose(qualifiedName);
        } else {
            writer.write(">");
            writer.write("\n");

            if (textOnly) {
                // we have at least one text node so lets assume
                // that its non-empty
                writeElementContent(element);
            } else {
                // we know it's not null or empty from above
                ++indentLevel;

                writeElementContent(element);

                --indentLevel;

                writePrintln();
                indent();
            }

            writer.write("</");
            writer.write(qualifiedName);
            writer.write(">");
            writer.write("\n");
        }
        // remove declared namespaceStack from stack
        while (namespaceStack.size() > previouslyDeclaredNamespaces) {
            namespaceStack.pop();
        }

        lastOutputNodeType = Node.ELEMENT_NODE;
    }

    @Override
    protected void writeEmptyElementClose(String qualifiedName)
            throws IOException {
        // Simply close up
        if (!format.isExpandEmptyElements()) {
            writer.write("/>");
            writer.write("\n");
        } else {
            writer.write("></");
            writer.write(qualifiedName);
            writer.write(">");
            writer.write("\n");
        }
    }
}