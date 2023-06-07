package com.alan344.component;

import cn.hutool.core.util.RandomUtil;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Region;
import javafx.stage.Modality;
import javafx.stage.Stage;
import org.kordamp.ikonli.javafx.FontIcon;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * @author AlanSun
 * @date 2023/2/4 0:10
 */
public class LeftRightLinkageBorderPane<GC extends LeftRightLinkageBorderPane.GroupName, GI extends LeftRightLinkageBorderPane.Item<GC>> extends BorderPane {
    private final ListView<GI> groupListView = new ListView<>();
    private final BorderPane borderPane = new BorderPane();
    private final Map<String, Region> listViewCache = new HashMap<>();
    private final Stage stage;
    private final Function<GC, GI> generatorGI;

    private List<GC> gcList;

    private final Function<GC, Region> rightNodeSupplier;

    public LeftRightLinkageBorderPane(Supplier<GC> generatorGc,
                                      Function<GC, GI> generatorGi,
                                      Function<GC, Region> rightNodeFunc,
                                      Stage stage,
                                      List<Button> bottomBtns,
                                      double... positions) {
        this.stage = stage;
        this.generatorGI = generatorGi;
        this.rightNodeSupplier = rightNodeFunc;
        this.getStylesheets().add("css/extra-file.css");

        SplitPane splitPane = new SplitPane(groupListView, borderPane);
        splitPane.setDividerPositions(positions);

        this.setCenter(splitPane);
        HBox hBox = new HBox(10);
        hBox.setPrefHeight(40);
        hBox.getChildren().addAll(bottomBtns);
        hBox.setStyle("-fx-padding: 0 10; -fx-background-color: #F7F8FA;");
        hBox.setAlignment(Pos.CENTER_RIGHT);
        this.setBottom(hBox);

        groupListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        groupListView.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> {
            if (null != newValue) {
                this.showRight(newValue.getConfig(), rightNodeFunc);
            }
        });
        groupListView.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            final GI selectedItem = groupListView.getSelectionModel().getSelectedItem();
            if (event.getButton() == MouseButton.SECONDARY) {
                // open context menu on current screen position
                MenuItem addMenuItem = new MenuItem("New");
                addMenuItem.setGraphic(new FontIcon("unil-plus-circle:16:BLUE"));
                addMenuItem.setOnAction(event1 -> this.openGroupConfigStage(null, false, groupName -> {
                    GC gc = generatorGc.get();
                    gc.setGroupName(groupName);
                    gc.setSystem(false);
                    gc.setList(new ArrayList<>(3));
                    GI gi = generatorGi.apply(gc);
                    if (gi instanceof Region) {
                        ((Region) gi).setPrefHeight(23);
                    }
                    gcList.add(gc);
                    groupListView.getItems().add(gi);
                    groupListView.getSelectionModel().select(gi);
                }));

                MenuItem updateMenuItem = new MenuItem("Edit");
                updateMenuItem.setGraphic(new FontIcon("unil-file-edit-alt:16:ORANGE"));
                updateMenuItem.setOnAction(event1 -> this.openGroupConfigStage(selectedItem.getName(), true, groupName -> {
                    selectedItem.setName(groupName);
                    final GC config = selectedItem.getConfig();
                    config.setGroupName(groupName);
                }));

                MenuItem copyMenuItem = new MenuItem("Copy");
                copyMenuItem.setGraphic(new FontIcon("unil-copy:16:GRAY"));
                copyMenuItem.setOnAction(event1 -> {
                    final GC gc = selectedItem.getConfig();
                    final GC cloneGc = (GC) gc.clone();
                    cloneGc.setGroupName(this.getGroupName(cloneGc.getGroupName() + "-COPY"));
                    cloneGc.setSystem(false);
                    GI cloneGi = generatorGi.apply(cloneGc);
                    if (cloneGi instanceof Region) {
                        ((Region) cloneGi).setPrefHeight(23);
                    }
                    gcList.add(cloneGc);
                    groupListView.getItems().add(groupListView.getSelectionModel().getSelectedIndex() + 1, cloneGi);
                    // 选中复制项
                    groupListView.getSelectionModel().select(cloneGi);
                });

                MenuItem deleteMenuItem = new MenuItem("Del");
                deleteMenuItem.setGraphic(new FontIcon("unil-times-circle:16:RED"));
                deleteMenuItem.setOnAction(event1 -> {
                    gcList.remove(selectedItem.getConfig());
                    groupListView.getItems().remove(selectedItem);
                });

                ContextMenu contextMenu = new ContextMenu(addMenuItem, updateMenuItem, copyMenuItem, deleteMenuItem);
                // 是否是系统内置的配置
                if (null == selectedItem) {
                    contextMenu = new ContextMenu(addMenuItem);
                } else if (selectedItem.getConfig().isSystem()) {
                    contextMenu = new ContextMenu(addMenuItem, copyMenuItem);
                }
                // 放入  contextMenu
                groupListView.setContextMenu(contextMenu);
            }
        });
    }

    private void showRight(GC gc, Function<GC, Region> rightNodeFunc) {
        final Region region = listViewCache.computeIfAbsent(gc.getGroupName(), s -> rightNodeFunc.apply(gc));
        borderPane.setCenter(region);
        groupListView.getItems().forEach(gi -> gi.getConfig().setEnable(false));
        gc.setEnable(true);
    }

    private String getGroupName(String newGroupName) {
        while (true) {
            String finalNewGroupName = newGroupName;
            final boolean isExist = groupListView.getItems().stream().anyMatch(gi -> gi.getName().equals(finalNewGroupName));
            if (isExist) {
                newGroupName = newGroupName + RandomUtil.randomLong(1, 999999999);
            } else {
                break;
            }
        }
        return newGroupName;
    }

    public void addLeftItems(List<GC> gcList) {
        this.gcList = gcList;
        // 展开第一个
        if (!gcList.isEmpty()) {

            // this.groupListView.pre
            this.groupListView.getItems().addAll(gcList.stream().map(gc -> {
                final GI gi = generatorGI.apply(gc);
                if (gi instanceof Region) {
                    ((Region) gi).setPrefHeight(25);
                }
                return gi;
            }).toList());

            // 打开之前使用的配置
            final Optional<GI> optionalGi = this.groupListView.getItems().stream()
                    .filter(gi -> gi.getConfig().isEnable()).findFirst();
            if (optionalGi.isPresent()) {
                final GI gi = optionalGi.get();
                final int i = this.groupListView.getItems().indexOf(gi);
                this.groupListView.getSelectionModel().select(i);
                this.borderPane.setCenter(rightNodeSupplier.apply(gi.getConfig()));
            } else {
                // 之前没打开过，选择第一个
                borderPane.setCenter(rightNodeSupplier.apply(gcList.get(0)));
                this.groupListView.getSelectionModel().select(0);
            }
        }
    }

    public BorderPane getRightBorderPane() {
        return this.borderPane;
    }

    public ListView<GI> getGroupLeftListView() {
        return this.groupListView;
    }

    /**
     * 打开添加/修改分组页面
     *
     * @param stringConsumer 分组名
     * @param isEdit         是否是编辑
     */
    private void openGroupConfigStage(String name, boolean isEdit, Consumer<String> stringConsumer) {
        Stage stage = new Stage();

        BorderPane borderPane = new BorderPane();
        borderPane.getStyleClass().add("border-pane-padding");
        borderPane.getStylesheets().add("css/common.css");
        borderPane.setPrefWidth(400);
        borderPane.setPrefHeight(150);

        // 分组名称
        Label label = new Label("分组名称: ");
        label.setPrefWidth(80);
        TextField textField = new TextField();
        if (null != name) {
            textField.setText(name);
        }
        textField.setPromptText("分组名称");
        textField.prefWidthProperty().bind(borderPane.prefWidthProperty().subtract(80));
        // 监听 enter 键
        textField.addEventHandler(KeyEvent.KEY_PRESSED, event -> {
            if (event.getCode() == KeyCode.ENTER) {
                stringConsumer.accept(textField.getText());
                stage.close();
            }
        });
        HBox nameText = new HBox(10, label, textField);
        nameText.setAlignment(Pos.CENTER);
        borderPane.setCenter(nameText);

        // 按钮
        Button cancelBtn = new Button("取消");
        cancelBtn.setOnAction(event -> stage.close());
        Button applyBtn = new Button("应用");
        applyBtn.setOnAction(event -> {
            stringConsumer.accept(textField.getText());
            stage.close();
        });
        HBox btnHbox = new HBox(10, cancelBtn, applyBtn);
        btnHbox.setAlignment(Pos.CENTER_RIGHT);
        borderPane.setBottom(btnHbox);

        stage.setScene(new Scene(borderPane));
        stage.setResizable(false);
        stage.getIcons().add(new Image("/image/icon.png"));
        stage.setTitle((isEdit ? "编辑" : "添加") + "分组");
        stage.initModality(Modality.WINDOW_MODAL);
        stage.initOwner(this.stage);
        stage.show();
    }

    public interface GroupName {
        String getGroupName();

        void setGroupName(String groupName);

        Collection<?> getList();

        void setList(Collection<?> list);

        boolean isSystem();

        void setSystem(boolean isSystem);

        boolean isEnable();

        void setEnable(boolean enable);

        Object clone();
    }

    public interface Item<G> {
        String getName();

        void setName(String name);

        G getConfig();
    }
}
