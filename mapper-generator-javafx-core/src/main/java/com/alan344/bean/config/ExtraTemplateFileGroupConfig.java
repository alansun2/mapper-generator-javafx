package com.alan344.bean.config;

import com.alan344.component.LeftRightLinkageBorderPane;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;

import java.util.Collection;
import java.util.UUID;

/**
 * @author AlanSun
 * @date 2023/1/30 15:58
 */
@Getter
@Setter
public class ExtraTemplateFileGroupConfig implements LeftRightLinkageBorderPane.GroupName, Cloneable {
    /**
     * 分组名称
     */
    private String groupName;

    private boolean isEnable;
    /**
     * 是否是系统内置的配置
     */
    private boolean isSystem;

    private Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList;

    @Override
    public ExtraTemplateFileGroupConfig clone() {
        final ExtraTemplateFileGroupConfig extraTemplateFileGroupConfig = JSON.parseObject(JSON.toJSONString(this), ExtraTemplateFileGroupConfig.class);
        final Collection<ExtraTemplateFileConfig> extraTemplateFileConfigList1 = extraTemplateFileGroupConfig.getExtraTemplateFileConfigList();
        extraTemplateFileConfigList1.forEach(extraTemplateFileConfig -> extraTemplateFileConfig.setId(UUID.randomUUID().toString()));
        return extraTemplateFileGroupConfig;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public Collection<ExtraTemplateFileConfig> getList() {
        return this.extraTemplateFileConfigList;
    }

    @JSONField(serialize = false, deserialize = false)
    @Override
    public void setList(Collection list) {
        this.extraTemplateFileConfigList = list;
    }
}
