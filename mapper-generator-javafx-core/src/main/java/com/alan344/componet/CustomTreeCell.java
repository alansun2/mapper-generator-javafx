package com.alan344.componet;

import com.alan344.bean.DataSource;
import com.jfoenix.controls.JFXTreeCell;
import javafx.css.PseudoClass;
import javafx.scene.Node;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.HBox;
import javafx.util.Callback;
import org.kordamp.ikonli.javafx.FontIcon;

/**
 * @author AlanSun
 * @date 2023/1/29 14:44
 * <p>
 * 自定义 tree cell 箭头
 **/
public class CustomTreeCell<T> extends JFXTreeCell<T> {

    private static final PseudoClass ROOT = PseudoClass.getPseudoClass("root");

    public static <T> Callback<TreeView<T>, TreeCell<T>> forTreeView() {
        return treeView -> new CustomTreeCell<>();
    }

    private HBox hbox;

    private void updateDisplay(T item, boolean empty) {
        if (item == null || empty) {
            hbox = null;
            setText(null);
            setGraphic(null);
        } else {
            TreeItem<T> treeItem = getTreeItem();
            if (treeItem != null && treeItem.getGraphic() != null) {
                if (item instanceof Node) {
                    setText(null);
                    if (hbox == null) {
                        hbox = new HBox(3);
                    }
                    hbox.getChildren().setAll(treeItem.getGraphic(), (Node) item);
                    setGraphic(hbox);
                } else {
                    hbox = null;
                    setText(item.toString());
                    setGraphic(treeItem.getGraphic());
                }
                this.setDisclosureNode(null);
                if (treeItem.getValue() instanceof DataSource) {
                    if (treeItem.isExpanded()) {
                        this.setDisclosureNode(new FontIcon("unim-angle-up:16:GRAY"));
                    } else {
                        this.setDisclosureNode(new FontIcon("unim-angle-down:16:GRAY"));
                    }
                } else {
                    this.setDisclosureNode(null);
                }
            } else {
                hbox = null;
                if (item instanceof Node) {
                    setText(null);
                    setGraphic((Node) item);
                } else {
                    setText(item.toString());
                    setGraphic(null);
                }
            }
        }
    }


    @Override
    protected void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);
        updateDisplay(item, empty);
        setMouseTransparent(item == null || empty);
    }
}