USE [master]
GO
/****** Object:  Database [historicalData]    Script Date: 24/05/2019 23:36:05 ******/
CREATE DATABASE [historicalData]
 CONTAINMENT = NONE
 ON  PRIMARY 
( NAME = N'historicalData', FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL14.SQLEXPRESS\MSSQL\DATA\historicalData.mdf' , SIZE = 73728KB , MAXSIZE = UNLIMITED, FILEGROWTH = 65536KB )
 LOG ON 
( NAME = N'historicalData_log', FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL14.SQLEXPRESS\MSSQL\DATA\historicalData_log.ldf' , SIZE = 270336KB , MAXSIZE = 2048GB , FILEGROWTH = 65536KB )
GO
ALTER DATABASE [historicalData] SET COMPATIBILITY_LEVEL = 140
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [historicalData].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [historicalData] SET ANSI_NULL_DEFAULT OFF 
GO
ALTER DATABASE [historicalData] SET ANSI_NULLS OFF 
GO
ALTER DATABASE [historicalData] SET ANSI_PADDING OFF 
GO
ALTER DATABASE [historicalData] SET ANSI_WARNINGS OFF 
GO
ALTER DATABASE [historicalData] SET ARITHABORT OFF 
GO
ALTER DATABASE [historicalData] SET AUTO_CLOSE ON 
GO
ALTER DATABASE [historicalData] SET AUTO_SHRINK OFF 
GO
ALTER DATABASE [historicalData] SET AUTO_UPDATE_STATISTICS ON 
GO
ALTER DATABASE [historicalData] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO
ALTER DATABASE [historicalData] SET CURSOR_DEFAULT  GLOBAL 
GO
ALTER DATABASE [historicalData] SET CONCAT_NULL_YIELDS_NULL OFF 
GO
ALTER DATABASE [historicalData] SET NUMERIC_ROUNDABORT OFF 
GO
ALTER DATABASE [historicalData] SET QUOTED_IDENTIFIER OFF 
GO
ALTER DATABASE [historicalData] SET RECURSIVE_TRIGGERS OFF 
GO
ALTER DATABASE [historicalData] SET  ENABLE_BROKER 
GO
ALTER DATABASE [historicalData] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO
ALTER DATABASE [historicalData] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO
ALTER DATABASE [historicalData] SET TRUSTWORTHY OFF 
GO
ALTER DATABASE [historicalData] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO
ALTER DATABASE [historicalData] SET PARAMETERIZATION SIMPLE 
GO
ALTER DATABASE [historicalData] SET READ_COMMITTED_SNAPSHOT OFF 
GO
ALTER DATABASE [historicalData] SET HONOR_BROKER_PRIORITY OFF 
GO
ALTER DATABASE [historicalData] SET RECOVERY SIMPLE 
GO
ALTER DATABASE [historicalData] SET  MULTI_USER 
GO
ALTER DATABASE [historicalData] SET PAGE_VERIFY CHECKSUM  
GO
ALTER DATABASE [historicalData] SET DB_CHAINING OFF 
GO
ALTER DATABASE [historicalData] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO
ALTER DATABASE [historicalData] SET TARGET_RECOVERY_TIME = 60 SECONDS 
GO
ALTER DATABASE [historicalData] SET DELAYED_DURABILITY = DISABLED 
GO
ALTER DATABASE [historicalData] SET QUERY_STORE = OFF
GO
USE [historicalData]
GO
/****** Object:  Schema [hist]    Script Date: 24/05/2019 23:36:05 ******/
CREATE SCHEMA [hist]
GO
/****** Object:  Table [hist].[Country]    Script Date: 24/05/2019 23:36:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [hist].[Country](
	[id] [uniqueidentifier] NOT NULL,
	[country] [nvarchar](20) NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [hist].[ELO]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [hist].[ELO](
	[id] [uniqueidentifier] NOT NULL,
	[teamId] [uniqueidentifier] NOT NULL,
	[startDate] [datetime] NULL,
	[endDate] [datetime] NULL,
	[rating] [decimal](18, 2) NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [hist].[League]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [hist].[League](
	[id] [uniqueidentifier] NOT NULL,
	[league] [nvarchar](32) NULL,
	[country_id] [uniqueidentifier] NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [hist].[Match]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [hist].[Match](
	[id] [uniqueidentifier] NOT NULL,
	[date] [nvarchar](200) NULL,
	[season] [nvarchar](200) NULL,
	[homeTeam_id] [uniqueidentifier] NULL,
	[awayTeam_id] [uniqueidentifier] NULL,
	[FTHG] [int] NULL,
	[FTAG] [int] NULL,
	[FTR] [char](1) NULL,
	[B365H] [decimal](10, 2) NULL,
	[B365D] [decimal](10, 2) NULL,
	[B365A] [decimal](10, 2) NULL,
	[BWH] [decimal](10, 2) NULL,
	[BWD] [decimal](10, 2) NULL,
	[BWA] [decimal](10, 2) NULL,
	[GBH] [decimal](10, 2) NULL,
	[GBD] [decimal](10, 2) NULL,
	[GBA] [decimal](10, 2) NULL,
	[IWH] [decimal](10, 2) NULL,
	[IWD] [decimal](10, 2) NULL,
	[IWA] [decimal](10, 2) NULL,
	[LBH] [decimal](10, 2) NULL,
	[LBD] [decimal](10, 2) NULL,
	[LBA] [decimal](10, 2) NULL,
	[PSH] [decimal](10, 2) NULL,
	[PSD] [decimal](10, 2) NULL,
	[PSA] [decimal](10, 2) NULL,
	[SBH] [decimal](10, 2) NULL,
	[SBD] [decimal](10, 2) NULL,
	[SBA] [decimal](10, 2) NULL,
	[SYH] [decimal](10, 2) NULL,
	[SYD] [decimal](10, 2) NULL,
	[SYA] [decimal](10, 2) NULL,
	[WHH] [decimal](10, 2) NULL,
	[WHD] [decimal](10, 2) NULL,
	[WHA] [decimal](10, 2) NULL,
	[VCH] [decimal](10, 2) NULL,
	[VCD] [decimal](10, 2) NULL,
	[VCA] [decimal](10, 2) NULL,
	[country] [uniqueidentifier] NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [hist].[Team]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [hist].[Team](
	[id] [uniqueidentifier] NOT NULL,
	[teamName] [nvarchar](200) NULL,
PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
ALTER TABLE [hist].[League]  WITH CHECK ADD FOREIGN KEY([country_id])
REFERENCES [hist].[Country] ([id])
GO
ALTER TABLE [hist].[Match]  WITH CHECK ADD FOREIGN KEY([awayTeam_id])
REFERENCES [hist].[Team] ([id])
GO
ALTER TABLE [hist].[Match]  WITH CHECK ADD FOREIGN KEY([homeTeam_id])
REFERENCES [hist].[Team] ([id])
GO
ALTER TABLE [hist].[Match]  WITH CHECK ADD  CONSTRAINT [FK_Match_Country] FOREIGN KEY([country])
REFERENCES [hist].[Country] ([id])
GO
ALTER TABLE [hist].[Match] CHECK CONSTRAINT [FK_Match_Country]
GO
/****** Object:  StoredProcedure [hist].[insertELO]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [hist].[insertELO] @team NVARCHAR(MAX), @startDate DATETIME , @endDate DATETIME, @rating DECIMAL
AS
INSERT INTO hist.ELO (id,teamId,startDate,endDate,rating)
VALUES ( NEWID(), 
		(SELECT id FROM hist.Team WHERE teamName = @team),
		 CONVERT(datetime,@startDate,103),
		 CONVERT(datetime,@endDate,103),
		 @rating
		 )
GO
/****** Object:  StoredProcedure [hist].[InsertMatch]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [hist].[InsertMatch] @date NVARCHAR(200), @season NVARCHAR(200), @country NVARCHAR(200), @homeTeam NVARCHAR(200), @awayTeam NVARCHAR(200), @FTHG INT, @FTAG INT, @FTR CHAR,
@B365H DECIMAL(10,2), @B365D DECIMAL(10,2), @B365A DECIMAL(10,2), @BWH DECIMAL(10,2),@BWD DECIMAL(10,2),@BWA DECIMAL(10,2),@GBH DECIMAL(10,2),@GBD DECIMAL(10,2),
	@GBA DECIMAL(10,2),@IWH DECIMAL(10,2),@IWD DECIMAL(10,2),@IWA DECIMAL(10,2),@LBH DECIMAL(10,2),@LBD DECIMAL(10,2),@LBA DECIMAL(10,2),@PSH DECIMAL(10,2),
	@PSD DECIMAL(10,2),@PSA DECIMAL(10,2),@SBH DECIMAL(10,2),@SBD DECIMAL(10,2),@SBA DECIMAL(10,2),@SYH DECIMAL(10,2),@SYD DECIMAL(10,2),@SYA DECIMAL(10,2),
	@WHH DECIMAL(10,2),@WHD DECIMAL(10,2),@WHA DECIMAL(10,2),@VCH DECIMAL(10,2),@VCD DECIMAL(10,2),@VCA DECIMAL(10,2)
AS
INSERT INTO Match(id,date,season,country,homeTeam_id,awayTeam_id,FTHG,FTAG,FTR,B365H,B365D,B365A,BWH,BWD,BWA,GBH,GBD,GBA,IWH,IWD,IWA,LBH,LBD,LBA,PSH,PSD,PSA,SBH,
					SBD,SBA,SYH,SYD,SYA,WHH,WHD,WHA,VCH,VCD,VCA)
VALUES
(NEWID(),@date,@season,(SELECT id FROM hist.Country WHERE country = @country),(SELECT id FROM Team WHERE teamName = @homeTeam), (SELECT id FROM Team WHERE teamName = @awayTeam),
@FTHG,@FTAG,@FTR,@B365H,@B365D,@B365A,@BWH,@BWD,@BWA,@GBH,@GBD,@GBA,@IWH,@IWD,@IWA,@LBH,@LBD,@LBA,@PSH,@PSD,@PSA,@SBH,@SBD,@SBA,@SYH,@SYD,@SYA,@WHH,@WHD,@WHA,@VCH,@VCD,@VCA)
GO
/****** Object:  StoredProcedure [hist].[InsertTeam]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [hist].[InsertTeam] @teamName NVARCHAR(200)
AS 
INSERT INTO hist.Team (id,teamName) VALUES (NEWID(),@teamName)
GO
/****** Object:  StoredProcedure [hist].[SearchTeam]    Script Date: 24/05/2019 23:36:06 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [hist].[SearchTeam]  @teamName NVARCHAR(200) 
AS
SELECT * FROM hist.Team WHERE teamName = @teamName
GO
USE [master]
GO
ALTER DATABASE [historicalData] SET  READ_WRITE 
GO
