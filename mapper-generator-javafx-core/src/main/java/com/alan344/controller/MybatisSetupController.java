package com.alan344.controller;

import com.alan344.bean.MybatisExportConfig;
import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.MybatisExportController;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.VBox;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.stereotype.Controller;

import javax.annotation.Resource;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

/**
 * @author AlanSun
 * @date 2019/8/15 17:55
 */
@Slf4j
@Controller
public class MybatisSetupController implements Initializable {
    @FXML
    private BorderPane mybatisSetupBorderPane;
    @FXML
    private ListView<String> setUpListView;
    @FXML
    private Button addBtn;
    @FXML
    private BorderPane setUpListBoardPane;
    @FXML
    private SplitPane splitPane;
    @Resource
    private ConfigService configService;
    @Resource
    private BeanFactory beanFactory;
    @Resource
    private MybatisExportController mybatisExportController;
    @Resource
    private ExportService exportService;
    @Resource
    private NodeHandler nodeHandler;

    /**
     * 配置信息 map
     */
    private Map<String, MybatisExportConfig> configNameConfigMap = new HashMap<>();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        addBtn.prefWidthProperty().bind(setUpListBoardPane.widthProperty());
        VBox exportVBox = FxmlLoadFactory.create("/fxml/component/mybatis-export-setup.fxml", beanFactory);
        exportVBox.setId("main-set-up");

        splitPane.getItems().add(exportVBox);

        // 加载配置文件
        List<MybatisExportConfig> mybatisExportConfigs = configService.loadConfigFromFile();

        if (!mybatisExportConfigs.isEmpty()) {
            mybatisExportConfigs.forEach(this::addConfigButton);
            // 显示第一个config
            mybatisExportController.showConfig(mybatisExportConfigs.get(0));

            this.configNameConfigMap = mybatisExportConfigs.stream().collect(Collectors.toMap(MybatisExportConfig::getConfigName, o -> o));
        }

        // 设置 listview 单选
        setUpListView.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        // 设置点击时间
        setUpListView.setOnMouseClicked(event -> {
            if (event.getButton() == MouseButton.PRIMARY) {
                mybatisExportController.showConfig(this.configNameConfigMap.get(((ListView<String>) event.getSource()).getSelectionModel().getSelectedItem()));
            }
        });

        // 添加右键功能
        MenuItem removeMenuItem = new MenuItem("删除");
        removeMenuItem.setOnAction(event -> {
            final String selectedItem = setUpListView.getSelectionModel().getSelectedItem();
            this.deleteConfig(selectedItem, this.configNameConfigMap.get(selectedItem));
        });
        setUpListView.setContextMenu(new ContextMenu(removeMenuItem));

        // 入栈
        nodeHandler.addNode(mybatisSetupBorderPane);
    }

    /**
     * 添加配置
     *
     * @param mybatisExportConfig 配置信息
     */
    public void addConfig(MybatisExportConfig mybatisExportConfig) {
        //写入文件
        int addType = configService.addConfig(mybatisExportConfig);
        //同时更新内存的配置信息
        if (addType == 3) {
            this.addConfigButton(mybatisExportConfig);
            this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
        } else if (addType == 1) {
            this.configNameConfigMap.put(mybatisExportConfig.getConfigName(), mybatisExportConfig);
        }
    }

    /**
     * 删除配置
     *
     * @param mybatisExportConfig 配置信息
     */
    private void deleteConfig(String configName, MybatisExportConfig mybatisExportConfig) {
        final ObservableList<String> items = setUpListView.getItems();
        int size = items.size();
        items.remove(configName);
        if (size == 1) {
            mybatisExportController.clearPane();
        } else {
            mybatisExportController.showConfig(configNameConfigMap.get(items.get(0)));
        }

        configService.deleteConfig(mybatisExportConfig);
    }

    /**
     * 添加配置文件有到左边的 listview
     *
     * @param mybatisExportConfig 配置信息
     */
    private void addConfigButton(MybatisExportConfig mybatisExportConfig) {
        setUpListView.getItems().add(mybatisExportConfig.getConfigName());
    }

    /**
     * 新增一个配置
     */
    @FXML
    public void addEmptyExportPane() {
        mybatisExportController.clearPane();
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    @FXML
    public void next() {
        mybatisExportController.validExport();

        Node next = nodeHandler.getNext();
        if (next == null) {
            next = FxmlLoadFactory.create("/fxml/service-list-view.fxml", beanFactory);
        }

        NodeConstants.borderPaneWrap.setCenter(next);
    }

    @FXML
    public void export() {
        mybatisExportController.validExport();
        exportService.export();
    }
}
