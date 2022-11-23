package com.alan344.service.node;

import javafx.scene.Node;

/**
 * @author AlanSun
 * @date 2020/9/8 10:29
 * <p>
 * node 的栈
 */
public class NodeStack {

    private NodeStack pre;

    private NodeStack next;

    private Node node;

    private boolean isFirst;

    public NodeStack getPre() {
        return pre;
    }

    public void setPre(NodeStack pre) {
        this.pre = pre;
    }

    public NodeStack getNext() {
        return next;
    }

    public void setNext(NodeStack next) {
        this.next = next;
    }

    public Node getNode() {
        return node;
    }

    public void setNode(Node node) {
        this.node = node;
    }

    public boolean isFirst() {
        return isFirst;
    }

    public void setFirst(boolean first) {
        isFirst = first;
    }
}
