package com.alan344.controller;

import com.alan344.bean.DataItem;
import com.alan344.bean.DataSource;
import com.alan344.bean.Table;
import com.alan344.service.DataSourceService;
import com.alan344.service.TableService;
import com.alan344.utils.Toast;
import javafx.application.HostServices;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import lombok.Getter;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

/**
 * @author AlanSun
 * @date 2019/8/7 17:04
 */
@Controller
public class MainController implements Initializable {
    @FXML
    private BorderPane borderPane;

    @FXML
    private MenuBar menuBar;

    @Getter
    @FXML
    private TreeItem<DataItem> treeItemRoot;

    @FXML
    private ListView<AnchorPane> anchorPaneListView;

    @FXML
    private TreeView<DataItem> treeViewDataSource;

    @Autowired
    private DateSourceController dateSourceController;

    @Autowired
    private DataSourceService dataSourceService;

    @Autowired
    private TableService tableService;

    @Autowired
    private BeanFactory beanFactory;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        menuBar.prefWidthProperty().bind(borderPane.widthProperty());

        treeViewDataSource.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

        treeViewDataSource.addEventHandler(MouseEvent.MOUSE_RELEASED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();

                //item is selected - this prevents fail when clicking on empty space
                ContextMenu contextMenu;
                if (selectedItems != null) {
                    //open context menu on current screen position
                    MenuItem menuItem = new MenuItem("导出");
                    menuItem.setOnAction(event1 -> export());
                    if (selectedItems.size() == 1 && selectedItems.get(0).getValue() instanceof DataSource) {

                        MenuItem menuItem2 = new MenuItem("修改数据源");
                        MenuItem menuItem3 = new MenuItem("删除数据源");
                        menuItem3.setOnAction(event1 -> deleteDataSource());
                        contextMenu = new ContextMenu(menuItem, menuItem2, menuItem3);
                    } else {
                        contextMenu = new ContextMenu(menuItem);
                    }
                    treeViewDataSource.setContextMenu(contextMenu);
                }
            }
        });

        //从文件加载数据源至pane
        dataSourceService.loadDataSourceFromFile(treeItemRoot);
    }

    /*---------------------------------------MenuBar------------------------------------------------------------------*/

    /**
     * 添加数据源
     */
    @FXML
    public void addSource() throws IOException {
        dateSourceController.addDataSource((Stage) borderPane.getScene().getWindow());
    }

    @FXML
    public void exit() {
        Platform.exit();
    }

    @FXML
    public void openGithub() {
        HostServices hostServices = beanFactory.getBean(HostServices.class);
        hostServices.showDocument("https://github.com/alansun2/mapper-generator-javafx");
    }

    /*------------------------------------TreeView ContextMenu--------------------------------------------------------*/

    /**
     * 全部导出
     */
    private void export() {
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        if (selectedItems.size() == 1) {
            TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
            if (dataItemTreeItem.getValue() instanceof DataSource) {
                //TODO 生成xml文件
                ObservableList<TreeItem<DataItem>> children = dataItemTreeItem.getChildren();
                if (!children.isEmpty()) {
                    children.forEach(itemTreeItem -> {
                        System.out.println(((Table) itemTreeItem.getValue()).getTableName());
                    });
                }
            } else {
                //TODO 生成xml文件
                Table table = (Table) dataItemTreeItem.getValue();
                System.out.println(table.getTableName());
            }
        } else {
            //TODO 生成xml文件
            List<Table> tables = new ArrayList<>();
            for (TreeItem<DataItem> selectedItem : selectedItems) {
                DataItem dataItem = selectedItem.getValue();
                if (dataItem instanceof Table) {
                    Table table = (Table) dataItem;
                    tables.add(table);
                    System.out.println(table.getTableName());
                }
            }

            tableService.setListView(tables, anchorPaneListView);
        }
    }

    /**
     * 删除数据源
     */
    private void deleteDataSource() {
        Stage primaryStage = (Stage) borderPane.getScene().getWindow();
        ObservableList<TreeItem<DataItem>> selectedItems = treeViewDataSource.getSelectionModel().getSelectedItems();
        if (selectedItems.size() != 1) {
            Toast.Companion.makeText(primaryStage, "只能选择一个", 3000, 500, 500, 15, 5);
            return;
        }

        TreeItem<DataItem> dataItemTreeItem = selectedItems.get(0);
        if (dataItemTreeItem.getValue() instanceof Table) {
            Toast.Companion.makeText(primaryStage, "无法对表进行操作", 3000, 500, 500, 15, 5);
            return;
        }

        treeItemRoot.getChildren().remove(dataItemTreeItem);

        dataSourceService.deleteDataSource(((DataSource) dataItemTreeItem.getValue()));
    }
}
