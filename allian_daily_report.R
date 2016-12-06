library(rJava)
library(xlsxjars)

read_table <- function(
    filename, sheet_num=1,
    sheet_name=NULL, filetype='xlsx') {

    if (filetype=='xlsx') {
        library(xlsx)
        if (is.null(sheet_name)) {
            data <- read.xlsx(filename, sheet_num,
                    fixed=TRUE, encoding='UTF-8')
            return(data)
        }
        else {
            data <- read.xlsx(filename, sheetName=sheet_name,
            fixed=TRUE, encoding='UTF-8')
            return(data)
        }
    }
}

read_basic_data <- function(table_name) {
    basic_data_dir <- 'D:/馔山数据/'
    if (table_name=='allian_structure') {
        table_content <- read_table(paste(
        basic_data_dir, '联营架构.xlsx', sep=''), 1)
        return(table_content)
    }

    if (table_name=='all_restaurant_info') {
        all_restaurant_info <- read_table(
                               paste(basic_data_dir,
                               '葵花宝典-联营编号精简.xlsx',
                               sep=''), 1)
        return(all_restaurant_info)
    }

}

read_tables <- function(report_date, table_name) {

    table_direct <- paste('D:/馔山数据/联营日报/',
                  report_date, '联营日报/', sep='')
    yesterday <- as.numeric(report_date)-1
    yesterday_KPI_direct <- paste(
                            'D:/馔山数据/联营日报/',
                            yesterday, '联营日报/', sep='')

    if (table_name=='allian_daily_report') {
        allian_daily_report <- read_table(
                    paste(daily_report_direct,
                    report_date,'联营日报.xlsx',
                    sep=''), 3)
        return(allian_daily_report)
    }

    if (table_name=='until_yesterday_cooperate_res') {
        until_yesterday_cooperate_res <- read_table(
                                      paste(yesterday_KPI_direct,
                                      yesterday,'联营KPI日报.xlsx',
                                      sep=''), 4)
        return(yesterday_cooperate_res)
    }

    if (table_name=='model_2_payed_restaurant') {
        model_2_payed_restaurant <- read_table(paste(table_direct,
                                    '联营2.0餐厅缴费统计.xlsx',
                                    sep=''), 1)
    }

    if (table_name=='change_state_restaurant') {
        change_state_restaurant <- read_table(
                                   paste(table_direct, report_date,
                                   'TS自营餐厅变更记录.xlsx',
                                   sep=''), 1)
        return(change_state_restaurant)
    }

    if (table_name=='soldout_food_info') {
    soldout_food_info <- read_table(
                         paste(table_direct, report_date,
                         'TS每日售罄数据.xlsx',
                         sep=''), 1)
    }

    if (table_name=='selled_food_info') {
    selled_food_info <- read_table(
                        paste(table_direct, report_date,
                        'SIA餐厅前端菜品信息.xlsx',
                        sep=''), 1)
    }


    if (table_name=='consistent_food_name') {
        consistent_food_name <- read_table(
                                'D:/馔山数据/更换菜品名称.xlsx',1)
    }

    if (table_name=='restaurant_open_interval') {
        restaurant_open_interval <- read_table(
                                    paste(table_direct,
                                    report_date,
                                    'TS联营营业时长与转化.xlsx',
                                    sep=''), 1)
        return(restaurant_open_interval)
    }

    if (table_name=='all_cooperate_restaurant') {
        table_content <- read_table(
                         paste(table_direct,
                         report_date,
                         '联营KPI日报.xlsx', sep=''), 4)
        return(table_content)
    }

    if (table_name=='income_restaurant') {
        table_content <- read_table(
                         paste(table_direct,
                         report_date,
                         'sia日报.xlsx', sep=''), 1)
        return(table_content)
    }

    if (table_name=='deliver_range_income') {
        table_content <- read_table(paste(table_direct, report_date,
                         'SIA单店配送范围交易额.xlsx', sep=''), 1)
        return(table_content)
    }

    if (table_name=='impressions') {
        table_content <- read_table(paste(table_direct, report_date,
                         '联营午晚高峰营业时长明细.xlsx', sep=''), 1)
        return(table_content)
    }

    if (table_name=='rice_amount') {
        table_content <- read_table(paste(table_direct, report_date,
                         '计算套餐饮料米饭的数量.xlsx', sep=''), 1)
        return(table_content)
    }

    else {
        return(paste('table name', table_name, 'not exist'))
    }
}

#读取报表调用的function
all_cooperate_restaurant <- function(report_date){

    all_cooperate_restaurant <- read_tables(
                                report_date, 'all_cooperate_restaurant')

    return(all_cooperate_restaurant)
}

income_visual_restaurant <- function(report_date) {
    all_income_visual_restaurant_info <- read_tables(
        report_date, 'income_restaurant')

    income_restaurant <- subset(all_income_visual_restaurant_info,
    as.numeric(as.character(total)) > 0)
    return(income_restaurant)
}

peaktime_open_interval <- function(
    cooperate_visual_restaurant_id, report_date) {
    restaurant_open_interval <- read_tables(
                                report_date,
                                'restaurant_open_interval')

    cooperate_restaurant_open_interval <-
        subset(restaurant_open_interval,
               restaurant_id %in% cooperate_visual_restaurant_id)
}
    
peaktime_open_interval_below_standard_table <- 
function(report_date, cooperate_visual_restaurant_id) {
    restaurant_open_interval <- read_tables(
                                report_date,
                                'restaurant_open_interval')

    cooperate_restaurant_open_interval <-


    peak_time_open_interval_below_standard <- 
        subset(cooperate_restaurant_open_interval,
        total_time>30 & high_time<119)
    return(peak_time_open_interval_below_standard)

}

#生成报表程序
create_report_tables <- function(report_date) {
    allian_brands=c('集食号', '土老冒黄焖鸡', '笑掌柜', '乙味屋', '轰咖', '辛生活', '周大虾')

print(paste(date(), '读取葵花宝典'))
    all_restaurant_info <-
        read_basic_data('all_restaurant_info')
print(paste(date(), '宝典读取完毕'))

print(paste(date(), '读取联营架构'))
    allian_structure <- read_basic_data('allian_structure')
print(paste(date(), '架构读取完毕'))

#获取有效门店明细
#    all_cooperate_restaurant <- read_tables(
#    report_date, 'all_cooperate_restaurant')
#
#    all_cooperate_restaurant_id <- all_cooperate_restaurant$门店编码
#
#    all_cooperate_restaurant_info <- subset(
#    all_restaurant_info,
#    实体店编号 %in% all_cooperate_restaurant_id
#    & 品牌 %in% allian_brands)
#
#    cooperate_visual_restaurant_id <- all_cooperate_restaurant_info$店铺ID

#生成sia日报
print(paste(date(), '读取日报'))
    income_sia_report <- income_visual_restaurant(
    report_date)
print(paste(date(), '日报读取完毕'))

    income_visual_restaurant_id = income_sia_report$id

    income_visual_restaurant_info = subset(all_restaurant_info,
    店铺ID %in% income_visual_restaurant_id)

#合并葵花宝典
print(paste(date(), '合并葵花宝典'))
    sia_report_with_info = merge(
    income_sia_report, income_visual_restaurant_info,
    by.x='id', by.y='店铺ID')
rm(income_sia_report)
rm(all_restaurant_info)
gc()

#读取单店配送范围交易额
print(paste(date(), '读取配范额'))
    deliver_range_income <- read_tables(report_date,
                            'deliver_range_income')
print(paste(date(), '配范额读取完毕'))

#合并配送范围交易额
print(paste(date(), '合并配范额'))
    sia_report_with_info = merge(
    sia_report_with_info, deliver_range_income,
    by='id', all.x=TRUE)
rm(deliver_range_income)

#读取曝光量
print(paste(date(), '读取曝光量'))
    impressions <- read_tables(report_date,
                            'impressions')
print(paste(date(), '曝光量读取完'))

#合并曝光量
print(paste(date(), '合并曝光量'))
    sia_report_with_info = merge(
    sia_report_with_info, impressions,
    by.a='id', by.b='restaurant_id', all.x=TRUE)
rm(impressions)

#读取米饭数量
print(paste(date(), '读取米饭量'))
    rice_amount <- read_tables(report_date,
                             'rice_amount')
print(paste(date(), '米饭量读取完'))

#合并米饭量
print(paste(date(), '合并米饭量'))
    sia_report_with_info = merge(
    sia_report_with_info, rice_amount,
    by.c='id', by.d='restaurant_id', all.x=TRUE)
rm(rice_amount)

#sia_report_with_info[sia_report_with_info=='NULL'] <- '#N/A'

print(paste(date(), '计算日报数据'))
    daily_report = within(sia_report_with_info, {
    产品收入  <-total-as.numeric(as.character(deliver_fee))
    客单价    <-产品收入/user_number
    配送费    <-as.numeric(as.character(deliver_fee))
    无效订单率<-invalid_ord_num/(vaild_ord_num+invalid_ord_num)
    曝光量    <-as.numeric(as.character(baoguang_num))
    用户点击率<-as.numeric(as.character(visit_uv))/曝光量
    访客数    <-as.numeric(as.character(visit_uv))
    用户转化率<-user_number/访客数
    平台新占比<-ele_new_user_number/user_number
    SIA新占比 <-sia_new_user_number/user_number
    餐厅接单  <-as.numeric(as.character(restaruant_accept))
    配送时长  <-as.numeric(as.character(delivery_time))
    订单总时间<-餐厅接单+配送时长 
    一月前购买<-as.numeric(as.character(user_number_1))
    一月内复购<-as.numeric(as.character(user_number_2))
    复购率    <-一月内复购/一月前购买
    总优惠    <-eleme_subsidy+res_subsidy
    平台补贴率<-eleme_subsidy/产品收入
    餐厅补贴率<-res_subsidy/产品收入
    总优惠力度<-总优惠/产品收入
    配送范围额<-as.numeric(as.character(total_amount))
    配送范围比<-产品收入/配送范围额
    米饭总量  <-as.numeric(0)
    米饭总量  <-as.numeric(as.character(米饭))+as.numeric(as.character(黄焖鸡米饭))+as.numeric(as.character(再来一碗白米饭))+as.numeric(as.character(客官再来碗饭))+as.numeric(as.character(承让承让米饭))
    #米饭总量  <-米饭+黄焖鸡米饭+再来一碗白米饭+客官再来碗饭+承让承让米饭
    门店品牌数<-''
    材料成本  <-''
    加入蜂鸟  <-'无'
    加入蜂鸟[as.numeric(as.character(type))==0] <- '否'
    加入蜂鸟[as.numeric(as.character(type))!=0] <- '是'
    })

print(paste(date(), '筛选日报'))
daily_report_final=subset(daily_report, select=c(虚拟名, id, city_name,
品牌, 实体店编号, total, 产品收入, vaild_ord_num, 客单价, invalid_ord_num,
无效订单率, 曝光量, 用户点击率, user_number, 访客数, 用户转化率,
ele_new_user_number, 平台新占比, sia_new_user_number,SIA新占比, yuding_num,
配送费, rating, 餐厅接单, 配送时长, 订单总时间,
一月前购买, 一月内复购, 复购率, eleme_subsidy, res_subsidy, 总优惠,
平台补贴率, 餐厅补贴率, 总优惠力度, 配送范围额, 配送范围比, 米饭总量,
材料成本, 门店品牌数, 加入蜂鸟))

print(paste(date(), '写日报'))
    write.xlsx(daily_report_final, paste('D:/', report_date,
               '联营日报草稿.xlsx', sep=''), sheetName='联营日报',
               row.names=FALSE, append=TRUE)
print(paste(date(), '任务完成'))
return(daily_report_final)
rm(daily_report_final)


    peaktime_open_interval <- peaktime_open_interval(report_date,
    cooperate_visual_restaurant_id)

    peaktime_open_interval_below_standard_table <-
    peaktime_open_interval_below_standard_table(report_date,
    cooperate_visual_restaurant_id)

    write.xlsx(peaktime_open_interval_below_standard_table,
               'D:/联营KPI日报.xlsx',
               sheetName='高峰期营业时长未达标',
               row.names=FALSE, append=TRUE)

}
