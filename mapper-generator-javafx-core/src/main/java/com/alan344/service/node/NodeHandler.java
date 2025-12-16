package com.alan344.service.node;

import javafx.scene.Node;
import org.springframework.util.function.SingletonSupplier;

/**
 * @author AlanSun
 * @since 2020/9/8 10:27
 */
public class NodeHandler {

    private NodeStack curNode;

    private boolean needCache;

    public NodeHandler() {

    }

    public void setNeedCache(boolean needCache) {
        this.needCache = needCache;
    }

    private static final NodeHandler NODE_HANDLER = new NodeHandler();

    public static NodeHandler getSingleTon(boolean needCache) {
        final NodeHandler nodeHandler1 = SingletonSupplier.of(() -> NODE_HANDLER).obtain();
        nodeHandler1.setNeedCache(needCache);
        return nodeHandler1;
    }

    public void addNode(Node node) {
        final NodeStack nodeStack = this.packageNode(node);
        this.addToTail(nodeStack);
        this.curNode = nodeStack;
    }

    public Node getPre() {
        final NodeStack pre = curNode.getPre();
        final Node node = pre.getNode();
        this.curNode = pre;
        if (!needCache) {
            pre.setNext(null);
        }
        return node;
    }

    public Node getNext() {
        if (null == curNode) {
            return null;
        }
        final NodeStack next = curNode.getNext();
        if (next == null) {
            return null;
        }
        final Node node = next.getNode();
        this.curNode = next;
        return node;
    }

    private void addToTail(NodeStack nodeStack) {
        if (curNode != null) {
            curNode.setNext(nodeStack);
            nodeStack.setPre(curNode);
        } else {
            nodeStack.setFirst(true);
        }
    }

    private NodeStack packageNode(Node node) {
        NodeStack nodeStack = new NodeStack();
        nodeStack.setNode(node);
        return nodeStack;
    }
}
