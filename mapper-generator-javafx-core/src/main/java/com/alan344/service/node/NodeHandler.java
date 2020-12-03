package com.alan344.service.node;

import javafx.scene.Node;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2020/9/8 10:27
 */
@Service
public class NodeHandler {

    private NodeStack curNode;

    public void addNode(Node node) {
        final NodeStack nodeStack = this.packageNode(node);
        this.addToTail(nodeStack);
        this.curNode = nodeStack;
    }

    private void addToTail(NodeStack nodeStack) {
        if (curNode != null) {
            curNode.setNext(nodeStack);
            nodeStack.setPre(curNode);
        }
    }

    private NodeStack packageNode(Node node) {
        NodeStack nodeStack = new NodeStack();
        nodeStack.setNode(node);
        return nodeStack;
    }

    public Node getPre() {
        final NodeStack pre = curNode.getPre();
        final Node node = pre.getNode();
        this.curNode = pre;
        return node;
    }

    public Node getNext() {
        final NodeStack next = curNode.getNext();
        if (next == null) {
            return null;
        }
        final Node node = next.getNode();
        this.curNode = next;
        return node;
    }
}
