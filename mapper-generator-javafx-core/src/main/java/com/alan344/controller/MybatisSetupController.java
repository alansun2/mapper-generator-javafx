package com.alan344.controller;

import com.alan344.constants.NodeConstants;
import com.alan344.controller.component.MybaitsExportController;
import com.alan344.factory.FxmlLoadFactory;
import com.alan344.service.ConfigService;
import com.alan344.service.ExportService;
import com.alan344.service.node.NodeHandler;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import lombok.extern.slf4j.Slf4j;
import org.mybatis.generator.my.config.MybatisExportConfig;
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
    private ListView<Button> setUpListView;
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
    private MybaitsExportController mybaitsExportController;
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
        splitPane.getItems().add(exportVBox);

        // 绑定文本框和tab的宽度
        final ObservableList<Node> hBoxs = exportVBox.getChildren();
        hBoxs.forEach(node -> {
            final HBox hBox = (HBox) node;
            final ObservableList<Node> children = hBox.getChildren();
            if (children.size() > 1) {
                final Node node1 = children.get(1);
                if (node1 instanceof TextField) {
                    ((TextField) node1).prefWidthProperty().bind(exportVBox.widthProperty().multiply(0.6));
                }
            } else if (children.size() == 1) {
                final Node node1 = children.get(0);
                if (node1 instanceof TabPane) {
                    ((TabPane) node1).prefWidthProperty().bind(exportVBox.widthProperty());
                }
            }
        });

        // 加载配置文件
        List<MybatisExportConfig> mybatisExportConfigs = configService.loadConfigFromFile();

        if (!mybatisExportConfigs.isEmpty()) {
            mybatisExportConfigs.forEach(this::addConfigButton);
            // 显示第一个config
            mybaitsExportController.showConfig(mybatisExportConfigs.get(0));

            this.configNameConfigMap = mybatisExportConfigs.stream().collect(Collectors.toMap(MybatisExportConfig::getConfigName, o -> o));
        }

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
    private void deleteConfig(Button button, MybatisExportConfig mybatisExportConfig) {
        final ObservableList<Button> items = setUpListView.getItems();
        int size = items.size();
        items.remove(button);
        if (size == 1) {
            mybaitsExportController.clearPane();
        } else {
            mybaitsExportController.showConfig(configNameConfigMap.get(items.get(0).getText()));
        }

        configService.deleteConfig(mybatisExportConfig);
    }

    /**
     * 添加配置文件有左边的button
     *
     * @param mybatisExportConfig 配置信息
     */
    private void addConfigButton(MybatisExportConfig mybatisExportConfig) {
        Button button = new Button(mybatisExportConfig.getConfigName());
        MenuItem removeMenuItem = new MenuItem("删除");
        removeMenuItem.setOnAction(event -> this.deleteConfig(button, mybatisExportConfig));
        button.setContextMenu(new ContextMenu(removeMenuItem));

        button.prefWidthProperty().bind(setUpListView.widthProperty());
        button.setOnAction(event -> mybaitsExportController.showConfig(this.configNameConfigMap.get(mybatisExportConfig.getConfigName())));
        setUpListView.getItems().add(button);
    }

    /**
     * 新增一个配置
     */
    @FXML
    public void addEmptyExportPane() {
        mybaitsExportController.clearPane();
    }

    @FXML
    public void pre() {
        NodeConstants.borderPaneWrap.setCenter(nodeHandler.getPre());
    }

    @FXML
    public void next() {
        mybaitsExportController.validExport();

        Node next = nodeHandler.getNext();
        if (next == null) {
            next = FxmlLoadFactory.create("/fxml/service-list-view.fxml", beanFactory);
        }

        NodeConstants.borderPaneWrap.setCenter(next);
    }

    @FXML
    public void export() {
        mybaitsExportController.validExport();
        exportService.export();
    }
}
