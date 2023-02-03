package com.alan344.componet;

import com.alan344.bean.DataItem;
import javafx.scene.control.Label;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.util.Callback;

/**
 * @author AlanSun
 * @date 2023/1/29 14:44
 * <p>
 * 自定义 tree cell 箭头
 **/
public class CustomTreeCell1 extends TreeCell<DataItem> {

    public static Callback<TreeView<DataItem>, TreeCell<DataItem>> forTreeView() {
        return treeView -> new CustomTreeCell1();
    }

    private HBox hbox;

    private void updateDisplay(DataItem item, boolean empty) {
        if (empty || item == null) {
            setGraphic(null);
            setText(null);
            this.setDisclosureNode(null);
        } else {
            setGraphic(new Label(item.toString()));
            setText(null);
            TreeItem<DataItem> treeItem = getTreeItem();
            if (treeItem == null || treeItem.getChildren().isEmpty()) {
                this.setDisclosureNode(null);
                return;
            } else {
                if (treeItem.isExpanded()) {
                    ImageView imageView = new ImageView("image/export-error.png");
                    imageView.setPreserveRatio(true);
                    imageView.setFitWidth(15);
                    this.setDisclosureNode(imageView);
                } else {
                    ImageView imageView = new ImageView("image/export-success.png");
                    imageView.setPreserveRatio(true);
                    imageView.setFitWidth(15);
                    this.setDisclosureNode(imageView);
                }
            }
        }
    }

    @Override
    protected void updateItem(DataItem item, boolean empty) {
        super.updateItem(item, empty);
        updateDisplay(item, empty);
    }
}