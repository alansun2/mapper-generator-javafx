package com.alan344.utils;

import com.github.javaparser.TokenRange;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.type.ReferenceType;
import com.github.javaparser.printer.DefaultPrettyPrinterVisitor;
import com.github.javaparser.printer.configuration.PrinterConfiguration;

import java.util.Iterator;
import java.util.Optional;

import static com.github.javaparser.utils.Utils.isNullOrEmpty;

/**
 * @author AlanSun
 * @date 2023/6/8 11:23
 */
public class MyVisitor extends DefaultPrettyPrinterVisitor {
    public MyVisitor(PrinterConfiguration configuration) {
        super(configuration);
    }

    @Override
    public void visit(MethodDeclaration n, Void arg) {
        final Optional<TokenRange> tokenRange = n.getTokenRange();
        if (tokenRange.isPresent()) {
            printer.print(tokenRange.get().toString());
        } else {
            printComment(n.getComment(), arg);
            printMemberAnnotations(n.getAnnotations(), arg);
            printModifiers(n.getModifiers());
            printTypeParameters(n.getTypeParameters(), arg);
            if (!isNullOrEmpty(n.getTypeParameters())) {
                printer.print(" ");
            }
            n.getType().accept(this, arg);
            printer.print(" ");
            n.getName().accept(this, arg);
            printer.print("(");
            n.getReceiverParameter().ifPresent(rp -> {
                rp.accept(this, arg);
                if (!isNullOrEmpty(n.getParameters())) {
                    printer.print(", ");
                }
            });
            if (!isNullOrEmpty(n.getParameters())) {
                for (final Iterator<Parameter> i = n.getParameters().iterator(); i.hasNext(); ) {
                    final Parameter p = i.next();
                    p.accept(this, arg);
                    if (i.hasNext()) {
                        printer.print(", ");
                    }
                }
            }
            printer.print(")");
            if (!isNullOrEmpty(n.getThrownExceptions())) {
                printer.print(" throws ");
                for (final Iterator<ReferenceType> i = n.getThrownExceptions().iterator(); i.hasNext(); ) {
                    final ReferenceType name = i.next();
                    name.accept(this, arg);
                    if (i.hasNext()) {
                        printer.print(", ");
                    }
                }
            }
            if (!n.getBody().isPresent()) {
                printer.print(";");
            } else {
                printer.print(" ");
                n.getBody().get().accept(this, arg);
            }
        }
    }
}
